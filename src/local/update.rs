use std::{fmt::Display, io};

use thiserror::Error;

macro_rules! impl_tryfrom_update {
    (@generate, $split:expr) => {{
        let mut iter = $split;
        match (iter.next(), iter.next(), iter.next()) {
            (Some(u), Some(b), None) => Ok(Update {
                username: u.to_owned(),
                branch: b.to_owned(),
            }),
            (Some(_), Some(_), Some(_)) => Err(UpdateParseError::MismatchLength(3)),
            (Some(_), None, _) => Err(UpdateParseError::MismatchLength(1)),
            (None, ..) => Err(UpdateParseError::MismatchLength(0)),
        }
    }};

    (file: $($type:ty),+ $(,)?) => {
        $(
            #[cfg(feature = "system")]
            impl TryFrom<$type> for Update {
                type Error = UpdateParseError;

                fn try_from(value: $type) -> Result<Self, Self::Error> {
                    let contents = std::fs::read_to_string(value)?;

                    impl_tryfrom_update!(@generate, contents.split(' '))
                }
            }
        )*
    };

    (direct: $($type:ty),+ $(,)?) => {
        $(
            impl TryFrom<$type> for Update {
                type Error = UpdateParseError;

                fn try_from(value: $type) -> Result<Self, Self::Error> {

                    impl_tryfrom_update!(@generate, value.split(' '))
                }
            }
        )*
    };
}

/// Handles [`/usr/share/pacstall/repo/update`](/usr/share/pacstall/repo/update) file contents.
///
/// This does not handle the file itself, only the format of the file. You may wish to write back
/// this struct into the file.
///
/// # Examples
///
/// Creating an [`Update`] from a string.
///
/// ```
/// # use libpacstall::local::update::Update;
/// let my_update = Update::new("Elsie19", "master").expect("Could not parse");
/// ```
///
/// Creating an [`Update`] from a [`Path`](`std::path::Path`).
///
/// ```no_run
/// # use std::path::Path;
/// # use libpacstall::local::update::{Update, UpdateParseError};
/// let path = Path::new("/usr/share/pacstall/repo/update");
///
/// let my_update = Update::try_from(path).expect("update file not in expected format");
/// ```
///
/// Writing back contents.
///
/// ```no_run
/// # use libpacstall::local::update::Update;
/// use std::fs::OpenOptions;
/// use std::io::Write;
///
/// let my_update = Update::new("pacstall", "develop").unwrap();
///
/// let mut file = OpenOptions::new()
///    .write(true)
///    .truncate(true)
///    .open("/usr/share/pacstall/repo/update")?;
///
/// writeln!(file, "{}", my_update)?;
///
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(PartialEq, Eq)]
pub struct Update {
    username: String,
    branch: String,
}

/// Errors that come from parsing [`Update`].
#[derive(Error, Debug)]
pub enum UpdateParseError {
    /// IO errors.
    #[error("io error")]
    Io(#[from] io::Error),
    /// Mismatched string split size.
    #[error("invalid length of split, expected `2`, got `{0}`")]
    MismatchLength(usize),
    /// Empty string.
    #[error("empty string")]
    EmptyString,
}

impl Default for Update {
    /// Defaults to `pacstall master`.
    fn default() -> Self {
        Self {
            username: String::from("pacstall"),
            branch: String::from("master"),
        }
    }
}

impl Display for Update {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.username, self.branch)
    }
}

impl_tryfrom_update!(file: &std::path::Path, std::path::PathBuf);
impl_tryfrom_update!(direct: &str, String);

impl Update {
    /// Make new [`Update`].
    ///
    /// # Errors
    /// Will error if either parameter passed is empty.
    pub fn new<S: Into<String>>(username: S, branch: S) -> Result<Self, UpdateParseError> {
        let username = username.into();
        let branch = branch.into();

        if username.is_empty() || branch.is_empty() {
            Err(UpdateParseError::EmptyString)
        } else {
            Ok(Self { username, branch })
        }
    }
    /// Return targeted update username.
    ///
    /// # Example
    ///
    /// ```
    /// # use libpacstall::local::update::Update;
    /// let my_update = Update::new("oklopfer", "master").unwrap();
    /// assert_eq!(my_update.username(), "oklopfer");
    /// ```
    #[must_use]
    pub fn username(&self) -> &str {
        &self.username
    }

    /// Return targeted update branch.
    ///
    /// # Example
    ///
    /// ```
    /// # use libpacstall::local::update::Update;
    /// let my_update = Update::new("oklopfer", "master").unwrap();
    /// assert_eq!(my_update.branch(), "master");
    /// ```
    #[must_use]
    pub fn branch(&self) -> &str {
        &self.branch
    }

    /// Update targeted username.
    pub fn set_username<S: Into<String>>(&mut self, new_username: S) {
        self.username = new_username.into();
    }

    /// Update targeted branch.
    pub fn set_branch<S: Into<String>>(&mut self, new_branch: S) {
        self.username = new_branch.into();
    }

    /// Move [`Update`] with branch as `master`.
    #[must_use]
    pub fn into_master(self) -> Self {
        Self {
            username: self.username,
            branch: String::from("master"),
        }
    }

    /// Move [`Update`] with branch as `develop`.
    #[must_use]
    pub fn into_develop(self) -> Self {
        Self {
            username: self.username,
            branch: String::from("develop"),
        }
    }
}
