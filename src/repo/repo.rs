//! Manage pacstall repos.

use std::{fmt::Display, fs, path::Path, str::FromStr};

use pest_consume::{match_nodes, Error, Parser};
use thiserror::Error;

use crate::repo::repo_type::PackageRepo;

use super::repo_type::{GitHubURL, RepoURL, SourceUrlError};

/// List of repositories on the system.
#[derive(Debug)]
pub struct PacstallRepo {
    /// List of repos.
    pub list: Vec<RepoEntry>,
}

/// Repository entry.
#[derive(Debug, Clone)]
pub struct RepoEntry {
    /// URL of repository.
    pub url: RepoURL,
    /// Optional alias.
    pub alias: Option<String>,
}

/// File to [`PacstallRepo`] error.
#[derive(Debug, Error)]
pub enum RepoFileError {
    #[error("Io Error")]
    IoError(#[from] std::io::Error),
    #[error("Pest parsing error: {0}")]
    Pest(#[from] pest::error::Error<Rule>),
}

impl Iterator for PacstallRepo {
    type Item = RepoEntry;

    fn next(&mut self) -> Option<Self::Item> {
        self.list.pop()
    }
}

impl TryFrom<Vec<&str>> for PacstallRepo {
    type Error = RepoFileError;
    fn try_from(value: Vec<&str>) -> Result<Self, Self::Error> {
        PacstallRepo::from_str(&value.join(" "))
    }
}

impl Display for PacstallRepo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.list
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Default for PacstallRepo {
    fn default() -> Self {
        Self {
            list: vec![RepoEntry {
                url: RepoURL::GitHub(GitHubURL {
                    username: "pacstall".to_string(),
                    repo: "pacstall-programs".to_string(),
                    branch: "master".to_string(),
                }),
                alias: Some(String::from("pacstall")),
            }],
        }
    }
}

impl Display for RepoEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(alias) = &self.alias {
            write!(f, "{} @{}", self.url.raw(), alias)
        } else {
            write!(f, "{}", self.url.raw())
        }
    }
}

impl PacstallRepo {
    pub fn open<P: AsRef<Path>>(value: P) -> Result<Self, RepoFileError> {
        let reffed = value.as_ref();
        let contents = match fs::read_to_string(reffed) {
            Ok(o) => o,
            Err(e) => return Err(RepoFileError::IoError(e)),
        };

        PacstallRepo::from_str(&contents)
    }
}

impl FromStr for PacstallRepo {
    type Err = RepoFileError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let inputs = ParseRepoFile::parse(Rule::program, &s)?;

        let input = inputs.single()?;

        ParseRepoFile::program(input).map_err(RepoFileError::Pest)
    }
}

#[derive(Parser)]
#[grammar_inline = r#"
program = { SOI ~ entry ~ (NEWLINE* ~ entry)* ~ NEWLINE* ~ EOI }

entry = ${ url ~ WHITESPACE+ ~ alias | url }

url = ${ (ASCII_ALPHANUMERIC | ":" | "/" | "." | "-" | "_")+ }

alias = ${ "@" ~ word }

word = !{ ASCII_ALPHANUMERIC+ }

WHITESPACE = _{ " " | "\t" }

NEWLINE = _{ "\n" | "\r\n" }
"#]
struct ParseRepoFile;

type Res<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;
#[pest_consume::parser]
impl ParseRepoFile {
    fn EOI(_input: Node) -> Res<()> {
        Ok(())
    }

    fn word(input: Node) -> Res<String> {
        Ok(input.as_str().to_string())
    }

    fn alias(input: Node) -> Res<String> {
        Ok(match_nodes!(input.into_children();
            [word(word)] => word,
        ))
    }

    fn url(input: Node) -> Res<String> {
        Ok(input.as_str().to_string())
    }

    fn entry(input: Node) -> Res<RepoEntry> {
        Ok(match_nodes!(input.into_children();
            // TODO: FIX THIS
            [url(url)] => RepoEntry { url: RepoURL::try_from(url).unwrap(), alias: None },
            [url(url), alias(alias)] => RepoEntry { url: RepoURL::try_from(url).unwrap(), alias: Some(alias) },
        ))
    }

    fn program(input: Node) -> Res<PacstallRepo> {
        Ok(match_nodes!(input.into_children();
            [entry(entry).., EOI(())] => PacstallRepo { list: entry.collect() },
        ))
    }
}
