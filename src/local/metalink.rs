use std::{env, path::PathBuf};
use thiserror::Error;

use url::Url;

/// Abstracts different platforms into a consistent formatting.
pub trait Metalink {
    /// Error when trying to convert from [`Url`].
    type Err;

    /// Attempt to convert from [`Url`].
    fn from_url(url: &Url) -> Result<Self, Self::Err>
    where
        Self: Sized;

    /// Get platform identifier.
    ///
    /// # Examples
    /// * GitHub -> `github`
    /// * GitLab -> `gitlab`
    /// * SourceHut -> `srht`
    fn platform(&self) -> &str;

    /// Return formatted short repo link.
    ///
    /// # Examples
    /// * <https://github.com/pacstall/pacstall> -> `pacstall/pacstall`
    /// * <https://git.sr.ht/~elsie/test> -> `elsie/test`
    fn user_repo(&self) -> String {
        if let Some(branch) = self.branch() {
            format!("{}/{}#{}", self.user(), self.repo(), branch)
        } else {
            format!("{}/{}", self.user(), self.repo())
        }
    }

    /// Get username of repository.
    fn user(&self) -> &str;

    /// Get repository name.
    fn repo(&self) -> &str;

    /// Get branch name.
    fn branch(&self) -> Option<&str>;

    /// Get pretty representation.
    ///
    /// Generally is `platform:user_repo()`.
    fn pretty(&self) -> String {
        format!("{}:{}", self.platform(), self.user_repo())
    }
}

/// Because we support file paths as "metalinks".
impl Metalink for PathBuf {
    type Err = ();

    fn from_url(url: &Url) -> Result<Self, Self::Err>
    where
        Self: Sized,
    {
        url.to_file_path()
    }

    fn pretty(&self) -> String {
        // We don't support Windows ;^)
        #[allow(deprecated)]
        if let Some(home) = env::home_dir() {
            if let Ok(stripped) = self.strip_prefix(&home) {
                return format!("~/{}", stripped.display());
            }
        }
        self.display().to_string()
    }

    fn branch(&self) -> Option<&str> {
        None
    }

    fn user_repo(&self) -> String {
        String::new()
    }

    fn user(&self) -> &str {
        ""
    }

    fn repo(&self) -> &str {
        match self.file_name() {
            Some(val) => val.to_str().expect("Valid unicode"),
            None => "/",
        }
    }

    fn platform(&self) -> &str {
        "file"
    }
}

/// GitHub metalink handling.
pub struct GitHubLink {
    url: Url,
    repo: String,
    user: String,
    branch: String,
}

/// Errors that occur when parsing metalinks.
#[derive(Debug, Error)]
pub enum MetaLinkError {
    /// Empty URL.
    #[error("empty path segments")]
    Empty,
    /// Component size mismatch.
    #[error("invalid size, expected `{expected}`, got `{got}`")]
    Size { expected: usize, got: usize },
    /// Platform mismatch.
    #[error("mismatched platform, expected `{expected}`, got `{got}`")]
    MismatchedPlatform { expected: String, got: String },
    /// Missing domain.
    #[error("missing domain")]
    MissingDomain,
}

impl Metalink for GitHubLink {
    type Err = MetaLinkError;

    fn from_url(url: &Url) -> Result<Self, Self::Err>
    where
        Self: Sized,
    {
        if let Some(domain) = url.domain() {
            if domain != "raw.githubusercontent.com" {
                return Err(MetaLinkError::MismatchedPlatform {
                    expected: String::from("raw.githubusercontent.com"),
                    got: domain.to_string(),
                });
            }
        } else {
            return Err(MetaLinkError::MissingDomain);
        }

        let path_segments = url
            .path_segments()
            .ok_or(MetaLinkError::Empty)?
            .collect::<Vec<_>>();

        if path_segments.len() != 3 {
            return Err(MetaLinkError::Size {
                expected: 3,
                got: path_segments.len(),
            });
        }

        Ok(Self {
            url: url.clone(),
            user: path_segments[0].to_string(),
            repo: path_segments[1].to_string(),
            branch: path_segments[2].to_string(),
        })
    }

    fn platform(&self) -> &str {
        "github"
    }

    fn user(&self) -> &str {
        &self.user
    }

    fn repo(&self) -> &str {
        &self.repo
    }

    fn branch(&self) -> Option<&str> {
        Some(&self.branch)
    }
}
