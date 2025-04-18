use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use url::Url;

/// A list of repositories, generally pulled from
/// [`/usr/share/pacstall/repo/pacstallrepo`](/usr/share/pacstall/repo/pacstallrepo).
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct PacstallRepos(Vec<PacstallRepo>);

/// A repository entry.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct PacstallRepo {
    url: Url,
    alias: Option<String>,
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
        for repo in self.iter() {
            if !first {
                write!(f, "\n")?;
            }
            write!(f, "{}", repo)?;
            first = false;
        }
        Ok(())
    }
}

/// Formats output according to how it would be parsed in pacstall.
impl Display for PacstallRepo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.alias {
            Some(alias) => write!(f, "{} @{}", self.url, alias),
            None => write!(f, "{}", self.url),
        }
    }
}
