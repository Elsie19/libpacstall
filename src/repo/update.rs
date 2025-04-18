use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
    io,
    path::{Path, PathBuf},
};

use thiserror::Error;

macro_rules! impl_tryfrom_update {
    (@generate, $split:expr) => {
        if $split.len() != 2 {
            Err(UpdateParseError::MismatchLength($split.len()))
        } else {
            Ok(Update {
                username: $split[0].to_owned(),
                branch: $split[1].to_owned(),
            })
        }
    };

    (file: $($type:ty),+ $(,)?) => {
        $(
            impl TryFrom<$type> for Update {
                type Error = UpdateParseError;

                fn try_from(value: $type) -> Result<Self, Self::Error> {
                    let contents = std::fs::read_to_string(value)?;
                    let split: Vec<&str> = contents.split(' ').collect();

                    impl_tryfrom_update!(@generate, split)
                }
            }
        )*
    };

    (direct: $($type:ty),+ $(,)?) => {
        $(
            impl TryFrom<$type> for Update {
                type Error = UpdateParseError;

                fn try_from(value: $type) -> Result<Self, Self::Error> {
                    let split: Vec<&str> = value.split(' ').collect();

                    impl_tryfrom_update!(@generate, split)
                }
            }
        )*
    };
}

/// Handles `/usr/share/pacstall/repo/update`.
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

impl_tryfrom_update!(file: &Path, PathBuf, &OsStr, OsString);
impl_tryfrom_update!(direct: &str, String);
