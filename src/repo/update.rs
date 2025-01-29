//! Manage the update file.

use pest_consume::{match_nodes, Parser};
use std::io::BufRead;
use std::path::PathBuf;
use std::str::FromStr;
use std::{fs::File, io, path::Path};
use thiserror::Error;

/// Pacstall update file.
#[derive(Debug, PartialEq)]
pub struct Update {
    /// GitHub username.
    pub username: String,
    /// GitHub branch.
    pub branch: String,
}

#[derive(Parser)]
#[grammar_inline = r#"
program = { SOI ~ entry ~ EOI }

entry = ${ #username = word ~ WHITESPACE ~ #branch = word }

word = { ASCII_ALPHANUMERIC+ }

WHITESPACE = _{ " " | "\t" }
"#]
struct ParseUpdate;

/// File to [`Update`] error.
#[derive(Debug, Error, PartialEq)]
pub enum UpdateFileError {
    #[error("The path is not valid: {0}")]
    InvalidPath(PathBuf),
    #[error("Could not parse contents: {0}")]
    InvalidParse(String),
    #[error("Pest parsing error")]
    Pest(#[from] pest::error::Error<Rule>),
}

impl Default for Update {
    fn default() -> Self {
        Self {
            username: "pacstall".into(),
            branch: "master".into(),
        }
    }
}

impl Update {
    /// Create a new update struct.
    pub fn new<S: Into<String>>(username: S, branch: S) -> Self {
        Self {
            username: username.into(),
            branch: branch.into(),
        }
    }

    /// Convinience function to open an update file.
    ///
    /// # Errors
    /// Will error with [`UpdateFileError`] if the file could not be opened, is not exactly one
    /// line, or could not be parsed.
    pub fn open<P: AsRef<Path>>(value: P) -> Result<Self, UpdateFileError> {
        let reffed = value.as_ref();
        let Ok(file) = File::open(reffed) else {
            return Err(UpdateFileError::InvalidPath(reffed.into()));
        };
        let mut lines = io::BufReader::new(file).lines();

        let first = match lines.next() {
            Some(o) => o,
            None => return Err(UpdateFileError::InvalidParse("Empty file".into())),
        }
        // I think this can't fail.
        .unwrap();

        if lines.next().is_some() {
            return Err(UpdateFileError::InvalidParse(
                "File is not one line long".into(),
            ));
        }

        Self::from_str(&first).map_err(UpdateFileError::Pest)
    }
}

type Res<T> = std::result::Result<T, pest::error::Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;
#[pest_consume::parser]
impl ParseUpdate {
    fn EOI(_input: Node) -> Res<()> {
        Ok(())
    }

    fn word(input: Node) -> Res<String> {
        Ok(input.as_str().to_string())
    }

    fn entry(input: Node) -> Res<Update> {
        Ok(match_nodes!(input.into_children();
            [word(username), word(branch)] => Update { username, branch },
        ))
    }

    fn program(input: Node) -> Res<Update> {
        Ok(match_nodes!(input.into_children();
            [entry(entry), EOI(())] => entry,
        ))
    }
}

impl FromStr for Update {
    type Err = pest::error::Error<Rule>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let inputs = ParseUpdate::parse(Rule::program, s)?;

        let input = inputs.single()?;

        ParseUpdate::program(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let text = "pacstall master";
        assert_eq!(
            Ok(Update {
                username: "pacstall".into(),
                branch: "master".into(),
            }),
            text.parse::<Update>()
        );
    }

    #[test]
    fn pacstall_file() {
        assert_eq!(
            Ok(Update {
                username: "pacstall".into(),
                branch: "develop".into(),
            }),
            Update::open("/usr/share/pacstall/repo/update")
        );
    }
}
