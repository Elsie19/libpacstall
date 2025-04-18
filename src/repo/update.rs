use std::{
    borrow::Cow,
    ffi::{OsStr, OsString},
    fmt::Display,
    io,
    path::{Path, PathBuf},
};

use thiserror::Error;

macro_rules! impl_tryfrom_update {
    (@parse $source:expr, Owned) => {{
        let mut parts = $source.splitn(3, ' ');
        let username = parts.next().ok_or(UpdateParseError::MismatchLength(0))?;
        let branch = parts.next().ok_or(UpdateParseError::MismatchLength(1))?;
        if parts.next().is_some() {
            Err(UpdateParseError::MismatchLength(3))
        } else {
            Ok(Update {
                username: Cow::Owned(username.to_owned()),
                branch: Cow::Owned(branch.to_owned()),
            })
        }
    }};

    (@parse $source:expr, Borrowed) => {{
        let mut parts = $source.splitn(3, ' ');
        let username = parts.next().ok_or(UpdateParseError::MismatchLength(0))?;
        let branch = parts.next().ok_or(UpdateParseError::MismatchLength(1))?;
        if parts.next().is_some() {
            Err(UpdateParseError::MismatchLength(3))
        } else {
            Ok(Update {
                username: Cow::Borrowed(username),
                branch: Cow::Borrowed(branch),
            })
        }
    }};

    (file: $($type:ty),+ $(,)?) => {
        $(
            impl<'a> TryFrom<$type> for Update<'a> {
                type Error = UpdateParseError;

                fn try_from(value: $type) -> Result<Self, Self::Error> {
                    let contents = std::fs::read_to_string(value)?;
                    impl_tryfrom_update!(@parse contents, Owned)
                }
            }
        )*
    };

    (direct: &str => Borrowed, $($rest:tt)*) => {
        impl<'a> TryFrom<&'a str> for Update<'a> {
            type Error = UpdateParseError;

            fn try_from(value: &'a str) -> Result<Self, Self::Error> {
                impl_tryfrom_update!(@parse value, Borrowed)
            }
        }

        impl_tryfrom_update!(direct: $($rest)*);
    };

    (direct: $type:ty => Owned, $($rest:tt)*) => {
        impl<'a> TryFrom<$type> for Update<'a> {
            type Error = UpdateParseError;

            fn try_from(value: $type) -> Result<Self, Self::Error> {
                impl_tryfrom_update!(@parse value, Owned)
            }
        }

        impl_tryfrom_update!(direct: $($rest)*);
    };

    (direct:) => {};
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
/// # use crate::libpacstall::repo::update::Update;
/// let my_update = match Update::try_from("Elsie19 master") {
///     Ok(o) => o,
///     Err(e) => {
///         eprintln!("Could not parse: {e}");
///         std::process::exit(1);
///     }
/// };
/// ```
///
/// Creating an [`Update`] from a [`Path`].
///
/// ```
/// # use std::path::Path;
/// # use crate::libpacstall::repo::update::{Update, UpdateParseError};
/// let path = Path::new("/usr/share/pacstall/repo/update");
///
/// let my_update = Update::try_from(path).expect("update file not in expected format");
/// ```
///
/// Writing back contents.
///
/// ```no_run
/// # use std::fs::OpenOptions;
/// # use libpacstall::repo::update::Update;
/// # use std::io::Write;
/// #
/// let my_update = Update::try_from("pacstall develop")?;
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
pub struct Update<'a> {
    username: Cow<'a, str>,
    branch: Cow<'a, str>,
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
}

impl Default for Update<'_> {
    /// Defaults to `pacstall master`.
    fn default() -> Self {
        Self {
            username: Cow::Borrowed("pacstall"),
            branch: Cow::Borrowed("master"),
        }
    }
}

impl Display for Update<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.username, self.branch)
    }
}

impl_tryfrom_update!(file: &Path, PathBuf, &OsStr, OsString);
impl_tryfrom_update!(direct: &str => Borrowed, String => Owned,);

impl<'a> Update<'a> {
    /// Return targeted update username.
    ///
    /// # Example
    ///
    /// ```
    /// # use crate::libpacstall::repo::update::Update;
    /// let my_update = Update::try_from("oklopfer master").unwrap();
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
    /// # use crate::libpacstall::repo::update::Update;
    /// let my_update = Update::try_from("oklopfer master").unwrap();
    /// assert_eq!(my_update.branch(), "master");
    /// ```
    #[must_use]
    pub fn branch(&self) -> &str {
        &self.branch
    }

    /// Update targeted username.
    pub fn set_username<S>(&mut self, new_username: S)
    where
        S: Into<Cow<'a, str>>,
    {
        self.username = new_username.into();
    }

    /// Update targeted branch.
    pub fn set_branch<S>(&mut self, new_branch: S)
    where
        S: Into<Cow<'a, str>>,
    {
        self.username = new_branch.into();
    }

    /// Copy new [`Update`] with branch as `master`.
    #[must_use]
    pub fn to_master(&self) -> Self {
        Self {
            username: self.username.clone(),
            branch: Cow::Borrowed("master"),
        }
    }

    /// Copy new [`Update`] with branch as `develop`.
    #[must_use]
    pub fn to_develop(&self) -> Self {
        Self {
            username: self.username.clone(),
            branch: Cow::Borrowed("develop"),
        }
    }
}
