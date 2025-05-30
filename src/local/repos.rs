use std::{
    fmt::{Debug, Display},
    fs::File,
    io::{BufRead, BufReader, Read},
    ops::{Deref, DerefMut},
    path::Path,
    str::FromStr,
};

use thiserror::Error;
use url::Url;

/// A list of repositories, generally pulled from
/// [`/usr/share/pacstall/repo/pacstallrepo`](/usr/share/pacstall/repo/pacstallrepo).
///
/// # Notes
/// This is a struct that implements [`Deref`]/[`DerefMut`]. The only reason this is so is because
/// this struct should really be handled exactly like a [`Vec`] of [`PacstallRepo`]. The only
/// difference is that this struct implements its own [`Display`] logic.
///
/// # Examples
///
/// ```no_run
/// # use libpacstall::local::repos::PacstallRepos;
/// # use std::path::Path;
/// # use std::fs::File;
/// # fn main() -> std::io::Result<()> {
/// let file = File::open(Path::new("/usr/share/pacstall/repo/pacstallrepo"))?;
/// let repos = match PacstallRepos::try_from(file) {
///     Ok(o) => o,
///     Err(e) => {
///         eprintln!("{e}");
///         std::process::exit(1);
///     }
/// };
/// # Ok(())
/// # }
/// ```
#[derive(Clone, PartialEq, Eq)]
pub struct PacstallRepos(Vec<PacstallRepo>);

/// A repository entry with a URL and an optional alias.
///
/// # Examples
///
/// ```
/// # use libpacstall::local::repos::PacstallRepo;
/// # use url::Url;
/// # fn main() -> Result<(), libpacstall::local::repos::RepoEntryError> {
/// let basic = "https://raw.githubusercontent.com/pacstall/pacstall/master".parse::<PacstallRepo>()?;
/// let with_alias = "https://raw.githubusercontent.com/pacstall/pacstall/master @pacstall".parse::<PacstallRepo>()?;
/// assert_eq!(with_alias.alias(), Some("pacstall"));
/// # Ok(())
/// # }
/// ```
#[derive(Clone, PartialEq, Eq)]
pub struct PacstallRepo {
    url: Url,
    alias: Option<String>,
}

/// Repo entry errors.
#[derive(Debug, Error)]
pub enum RepoEntryError {
    #[error("missing URL")]
    MissingUrl,
    #[error("missing @ sign")]
    MissingAtSign,
    #[error("too many parts")]
    TooManyParts,
    #[error(transparent)]
    ParseError(#[from] url::ParseError),
    #[error("empty alias")]
    EmptyAlias,
    #[error("path is not absolute")]
    NotAbsolute,
    #[error("not a path")]
    NotPath,
}

/// Parsing repo file errors.
#[derive(Debug, Error)]
pub enum RepoFileParseError {
    #[error("parse error")]
    ParseError(#[from] RepoEntryError),
    #[error("io error")]
    IoError(#[from] std::io::Error),
}

impl Deref for PacstallRepos {
    type Target = Vec<PacstallRepo>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for PacstallRepos {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Pretty printing of repo list.
impl Display for PacstallRepos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for repo in &self.0 {
            if !first {
                writeln!(f)?;
            }
            write!(f, "{repo}")?;
            first = false;
        }
        Ok(())
    }
}

/// Formats output according to how it would be parsed in pacstall.
impl Display for PacstallRepo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.alias {
            Some(alias) => write!(f, "{} @{alias}", self.url),
            None => write!(f, "{}", self.url),
        }
    }
}

impl PartialEq<Url> for PacstallRepo {
    fn eq(&self, other: &Url) -> bool {
        self.url == *other
    }
}

impl Default for PacstallRepo {
    fn default() -> Self {
        Self {
            url: Url::parse("https://raw.githubusercontent.com/pacstall/pacstall-programs/master")
                .expect("could not parse default URL"),
            alias: Some(String::from("pacstall")),
        }
    }
}

impl Default for PacstallRepos {
    fn default() -> Self {
        Self(vec![PacstallRepo::default()])
    }
}

impl From<PacstallRepo> for PacstallRepos {
    fn from(value: PacstallRepo) -> Self {
        Self(vec![value])
    }
}

impl FromIterator<PacstallRepo> for PacstallRepos {
    fn from_iter<T: IntoIterator<Item = PacstallRepo>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for PacstallRepos {
    type Item = PacstallRepo;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromStr for PacstallRepo {
    type Err = RepoEntryError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split_whitespace();
        let url = Url::parse(parts.next().ok_or(Self::Err::MissingUrl)?)?;

        match parts.next() {
            Some(alias) => {
                if !alias.starts_with('@') {
                    return Err(Self::Err::MissingAtSign);
                }
                if parts.next().is_some() {
                    return Err(Self::Err::TooManyParts);
                }
                Ok(Self {
                    url,
                    alias: Some(alias.strip_prefix('@').unwrap().to_string()),
                })
            }
            None => Ok(Self { url, alias: None }),
        }
    }
}

impl TryFrom<File> for PacstallRepos {
    type Error = RepoFileParseError;

    fn try_from(value: File) -> Result<Self, Self::Error> {
        Self::open(value)
    }
}

impl PacstallRepo {
    /// Initialize a repo from a [`Url`].
    pub fn from_url<S: Into<String>>(url: Url, alias: Option<S>) -> Self {
        Self {
            url,
            alias: alias.map(Into::into),
        }
    }

    /// Initialize a repo from a [`Path`].
    ///
    /// # Errors
    /// Will error if [`Url::from_directory_path`] fails.
    pub fn from_path<P, S>(path: P, alias: Option<S>) -> Result<Self, ()>
    where
        P: AsRef<Path>,
        S: Into<String>,
    {
        Ok(Self {
            url: Url::from_directory_path(path)?,
            alias: alias.map(Into::into),
        })
    }

    /// Set new alias and returns old one.
    ///
    /// # Errors
    /// Will error if input `alias` is empty.
    pub fn set_alias<S: Into<String>>(
        &mut self,
        alias: S,
    ) -> Result<Option<String>, RepoEntryError> {
        let old = self.alias.clone();
        let alias: String = alias.into();
        if alias.is_empty() {
            Err(RepoEntryError::EmptyAlias)
        } else {
            self.alias = Some(alias);
            Ok(old)
        }
    }

    /// Check if repo entry has an alias.
    #[must_use]
    pub fn has_alias(&self) -> bool {
        self.alias.is_some()
    }

    /// Return alias.
    #[must_use]
    pub fn alias(&self) -> Option<&str> {
        self.alias.as_deref()
    }

    /// Return url.
    #[must_use]
    pub fn url(&self) -> &Url {
        &self.url
    }

    /// Check if the repo entry url is a path.
    #[must_use]
    pub fn is_path(&self) -> bool {
        self.url.scheme() == "file"
    }

    /// Return url as [`Path`].
    ///
    /// # Errors
    /// Will error if the url is not a path.
    pub fn as_path(&self) -> Result<&Path, RepoEntryError> {
        if self.is_path() {
            Ok(Path::new(self.url.path()))
        } else {
            Err(RepoEntryError::NotPath)
        }
    }
}

impl PacstallRepos {
    /// Create new [`PacstallRepos`] from readable buffer.
    ///
    /// # Errors
    ///
    /// Will error if the repo file cannot be parsed properly.
    pub fn open<R: Read>(contents: R) -> Result<Self, RepoFileParseError> {
        Ok(Self(
            BufReader::new(contents)
                .lines()
                .map(|line| {
                    line.map_err(RepoFileParseError::IoError)?
                        .parse()
                        .map_err(RepoFileParseError::ParseError)
                })
                .collect::<Result<_, _>>()?,
        ))
    }
}
