//! Work with pacstall-aware repository types.
//!
//! Module for supported repository types in Pacstall, such as GitHub, GitLab, raw paths, etc.

use std::{path::PathBuf, str::FromStr};
use thiserror::Error;

use url::Url;

/// Implement types of repositories.
///
/// Repositories must have three methods of displaying themselves:
///
/// 1. [`PackageRepo::pretty()`] (user facing url/file/whatever).
/// 2. [`PackageRepo::raw()`] (backend facing url/file/whatever).
/// 2. [`PackageRepo::search()`] (styled and fancy).
///
/// `Raw` should be the expanded url/path to which paths can be appended to in the style of a
/// pacstall-programs repository.
///
/// If there isn't any `Pretty` option for the repository (such as a fileserver on a website), make
/// both `Pretty` and `Raw` the same.
pub trait PackageRepo {
    /// Pretty print the URL, generally the user facing repo.
    fn pretty(&self) -> String;
    /// Raw print the URL for appending paths to.
    fn raw(&self) -> String;
    /// Raw print the URL for appending paths to.
    fn search(&self) -> String;
    /// Adding paths to raw URL.
    fn with_path<S: AsRef<str>>(&self, path: S) -> String {
        let as_url = Url::parse(&self.raw()).unwrap();
        format!("{}", as_url.join(path.as_ref()).unwrap())
    }
}

#[derive(Debug, Error, PartialEq)]
/// Error type for parsing source URLs.
pub enum SourceUrlError {
    #[error("invalid host for parsed type (expected {expected:?}, found {found:?})")]
    /// Invalid host, such as trying to parse a GitLab URL into [`GitHubURL`].
    InvalidHost { expected: String, found: String },
    #[error("invalid slug for parsed type (expected {expected:?}, found {found:?})")]
    /// Invalid slug, meaning the expected syntax of the website path down to the expected append
    /// path could not be parsed properly.
    InvalidSlug {
        expected: Vec<String>,
        found: Vec<String>,
    },
    #[error("could not parse url")]
    /// A URL couldn't be parsed from the input.
    InvalidUrlParse(#[from] url::ParseError),
    #[error("could not convert to known type: {0}")]
    InvalidParse(String),
}

/// List of available repos that pacstall supports.
#[derive(Debug, Clone, PartialEq)]
pub enum RepoURL {
    /// GitHub.
    GitHub(GitHubURL),
    /// GitLab.
    GitLab(GitLabURL),
    /// Raw files.
    File(PathURL),
    /// Raw URLs.
    URL(RawURL),
}

/// GitHub URLs.
#[derive(PartialEq, Debug, Clone)]
pub struct GitHubURL {
    pub username: String,
    pub repo: String,
    pub branch: String,
}

/// GitLab URLs.
#[derive(PartialEq, Debug, Clone)]
pub struct GitLabURL {
    pub username: String,
    pub repo: String,
    pub branch: String,
}

/// Raw paths.
#[derive(PartialEq, Debug, Clone)]
pub struct PathURL {
    pub path: PathBuf,
}

/// Raw URLs.
#[derive(PartialEq, Debug, Clone)]
pub struct RawURL {
    pub url: Url,
}

impl TryFrom<String> for RepoURL {
    type Error = SourceUrlError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.starts_with("https://raw.githubusercontent.com") {
            Ok(RepoURL::GitHub(value.parse::<GitHubURL>()?))
        } else if value.starts_with("https://gitlab.com") {
            Ok(RepoURL::GitLab(value.parse::<GitLabURL>()?))
        } else if value.starts_with("http") {
            Ok(RepoURL::URL(value.parse::<RawURL>()?))
        } else if value.starts_with("file://") || value.starts_with('/') {
            Ok(RepoURL::File(value.parse::<PathURL>()?))
        } else {
            Err(SourceUrlError::InvalidParse(value))
        }
    }
}

impl PackageRepo for RepoURL {
    fn raw(&self) -> String {
        match self {
            Self::GitLab(m) => m.raw(),
            Self::GitHub(m) => m.raw(),
            Self::URL(m) => m.raw(),
            Self::File(m) => m.raw(),
        }
    }

    fn pretty(&self) -> String {
        match self {
            Self::GitLab(m) => m.pretty(),
            Self::GitHub(m) => m.pretty(),
            Self::URL(m) => m.pretty(),
            Self::File(m) => m.pretty(),
        }
    }

    fn search(&self) -> String {
        match self {
            Self::GitLab(m) => m.search(),
            Self::GitHub(m) => m.search(),
            Self::URL(m) => m.search(),
            Self::File(m) => m.search(),
        }
    }
}

impl PackageRepo for GitHubURL {
    fn raw(&self) -> String {
        format!(
            "https://raw.githubusercontent.com/{}/{}/{}",
            self.username, self.repo, self.branch
        )
    }

    fn pretty(&self) -> String {
        format!(
            "https://github.com/{}/{}/blob/{}",
            self.username, self.repo, self.branch
        )
    }

    fn search(&self) -> String {
        format!("github:{}/{}", self.username, self.repo)
    }
}

impl FromStr for GitHubURL {
    type Err = SourceUrlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = match Url::parse(s) {
            Ok(o) => o,
            Err(e) => return Err(SourceUrlError::InvalidUrlParse(e)),
        };

        if let Some(host) = url.host_str() {
            if host != "raw.githubusercontent.com" {
                return Err(SourceUrlError::InvalidHost {
                    expected: "raw.githubusercontent.com".to_string(),
                    found: host.to_string(),
                });
            }
        } else {
            return Err(SourceUrlError::InvalidHost {
                expected: "raw.githubusercontent.com".to_string(),
                found: String::new(),
            });
        }

        let Some(slugs) = url.path_segments() else {
            return Err(SourceUrlError::InvalidSlug {
                expected: vec!["username".into(), "repository".into(), "branch".into()],
                found: vec![],
            });
        };

        let mut split = slugs
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>();

        // We don't care about the final slug if it's empty. It's the difference between:
        // https://pacstall.dev/foo/bar/
        // https://pacstall.dev/foo/bar
        if let Some(last) = split.last() {
            if last.is_empty() {
                split.pop();
            }
        }

        if split.len() != 3 {
            return Err(SourceUrlError::InvalidSlug {
                expected: vec!["username".into(), "repository".into(), "branch".into()],
                found: split,
            });
        }

        Ok(Self {
            username: split[0].clone(),
            repo: split[1].clone(),
            branch: split[2].clone(),
        })
    }
}

impl PackageRepo for GitLabURL {
    fn raw(&self) -> String {
        format!(
            "https://gitlab.com/{}/{}/-/raw/{}",
            self.username, self.repo, self.branch
        )
    }

    fn pretty(&self) -> String {
        format!(
            "https://gitlab.com/{}/{}/-/tree/{}",
            self.username, self.repo, self.branch
        )
    }

    fn search(&self) -> String {
        format!("gitlab:{}/{}", self.username, self.repo)
    }
}

impl FromStr for GitLabURL {
    type Err = SourceUrlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = match Url::parse(s) {
            Ok(o) => o,
            Err(e) => return Err(SourceUrlError::InvalidUrlParse(e)),
        };

        let Some(slugs) = url.path_segments() else {
            return Err(SourceUrlError::InvalidSlug {
                expected: vec![
                    "username".into(),
                    "repository".into(),
                    "-".into(),
                    "raw".into(),
                    "branch".into(),
                ],
                found: vec![],
            });
        };

        let mut split = slugs
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>();

        // We don't care about the final slug if it's empty. It's the difference between:
        // https://pacstall.dev/foo/bar/
        // https://pacstall.dev/foo/bar
        if let Some(last) = split.last() {
            if last.is_empty() {
                split.pop();
            }
        }

        if split.len() != 5 {
            return Err(SourceUrlError::InvalidSlug {
                expected: vec![
                    "username".into(),
                    "repository".into(),
                    "-".into(),
                    "raw".into(),
                    "branch".into(),
                ],
                found: split,
            });
        }

        Ok(Self {
            username: split[0].clone(),
            repo: split[1].clone(),
            branch: split[4].clone(),
        })
    }
}

impl PackageRepo for PathURL {
    fn raw(&self) -> String {
        format!("file://{}", self.path.display())
    }

    fn pretty(&self) -> String {
        format!("{}", self.path.display())
    }

    fn search(&self) -> String {
        format!("{}", self.path.display())
    }
}

impl FromStr for PathURL {
    type Err = url::ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            path: PathBuf::from(s),
        })
    }
}

impl PackageRepo for RawURL {
    fn raw(&self) -> String {
        self.url.to_string()
    }

    fn pretty(&self) -> String {
        self.url.to_string()
    }

    fn search(&self) -> String {
        self.url.to_string()
    }
}

impl FromStr for RawURL {
    type Err = url::ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match Url::parse(s) {
            Ok(url) => Ok(Self { url }),
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_gitlab_string() {
        let text = "https://gitlab.com/porky11/langen/-/raw/master/";
        assert_eq!(
            text.parse::<GitLabURL>(),
            Ok(GitLabURL {
                username: "porky11".to_string(),
                repo: "langen".to_string(),
                branch: "master".to_string(),
            })
        );
    }

    #[test]
    fn parse_github_string() {
        let text = "https://raw.githubusercontent.com/pacstall/pacstall-programs/master/";
        assert_eq!(
            text.parse::<GitHubURL>(),
            Ok(GitHubURL {
                username: "pacstall".to_string(),
                repo: "pacstall-programs".to_string(),
                branch: "master".to_string(),
            })
        );
    }
}
