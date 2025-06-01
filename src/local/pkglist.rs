//! Handles the `packagelist` format and related tasks such as searching and filtering.

use std::{fmt::Display, fs};

use colored::{Color, ColoredString, Colorize};
use thiserror::Error;
use url::Url;

use super::{metalink::metalink, repos::PacstallRepos};

macro_rules! vte_format {
    ($link:expr, $($arg:tt)*) => {
        format!("\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\", $link, format!($($arg)*))
    };
}

/// Handle for converting repositories into [`PkgList`].
#[derive(Clone, PartialEq, Eq)]
pub struct Search(PacstallRepos);

/// Holds the packagelist.
///
/// An example `packagelist` can be found at <https://github.com/pacstall/pacstall-programs/blob/master/packagelist>.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct PkgList {
    contents: Vec<PkgBase>,
}

/// Holds a total package.
#[derive(Clone, PartialEq, Eq)]
pub struct PkgBase {
    pkgbase: String,
    packages: Vec<PackageReference>,
}

/// A package reference.
///
/// Holds the name of a package, it's repository and it's pacscript URL.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PackageReference {
    name: String,
    repo: Url,
    pacscript: Url,
}

/// Used for filtering out package names.
///
/// See [`PkgList::filter_pkg`].
pub struct FilterPkg<'a> {
    search: &'a str,
    pkgs: &'a [PkgBase],
}

/// Pretty format for package entries.
pub struct PkgDisplayEntry {
    pub pkgbase: Option<String>,
    pub name: String,
    // `source` comes from `metalink()`.
    pub source: (String, String),
}

/// Errors arising from trying to get and parse a `packagelist`.
///
/// See [`Search::into_pkglist`].
#[derive(Debug, Error)]
pub enum GetPkglistError {
    /// Could not read a `packagelist` from a local repository.
    #[error("could not read from file")]
    IoError(#[from] std::io::Error),

    /// Could not download a remote `packagelist`.
    #[error("could not download packagelist")]
    QueryError(#[from] reqwest::Error),

    /// Could not parse the repo entry URL.
    #[error("could not parse url")]
    UrlError(#[from] url::ParseError),

    /// Invalid line in the `packagelist`.
    #[error("invalid line in pkglist: `{0}`")]
    InvalidLine(String),

    /// Missing parent for child package.
    ///
    /// # Example
    ///
    /// ```packagelist
    /// parent:child1
    /// parent:child2
    /// ```
    ///
    /// A working example would be:
    ///
    /// ```packagelist
    /// parent:pkgbase
    /// parent:child1
    /// parent:child2
    /// ```
    #[error("missing parent pkgbase for: `{0}`")]
    MissingParent(String),
}

/// Colors for formatting [`PkgDisplayEntry`].
pub struct PackageColors {
    pub pkgbase: Color,
    pub package: Color,
    pub at_sign: Color,
    pub source: Color,
}

impl From<PacstallRepos> for Search {
    fn from(value: PacstallRepos) -> Self {
        Self(value)
    }
}

impl Search {
    /// Queries the remote `packagelist` and converts into a [`PkgList`].
    ///
    /// # Errors
    ///
    /// Will error if the `packagelist` is not reachable, unparsable, unopenable, or malformed.
    pub async fn into_pkglist(self) -> Result<PkgList, GetPkglistError> {
        let mut pkglist = PkgList::default();

        for entry in self.0 {
            let url = entry.url();
            let list = if let Ok(path) = url.to_file_path() {
                let list_path = path.join("packagelist");
                fs::read_to_string(list_path)?
            } else {
                let url = format!("{url}/packagelist");
                reqwest::get(&url).await?.text().await?
            };

            for pkg_entry in list.trim().lines() {
                let parts: Vec<_> = pkg_entry.split(':').collect();

                match parts.as_slice() {
                    [pkgbase] => {
                        pkglist.contents.push(PkgBase {
                            pkgbase: (*pkgbase).to_string(),
                            packages: vec![PackageReference {
                                name: (*pkgbase).to_string(),
                                repo: url.clone(),
                                pacscript: format!("{url}/packages/{pkgbase}/{pkgbase}.pacscript")
                                    .parse()?,
                            }],
                        });
                    }
                    [pkg, "pkgbase"] => {
                        pkglist.contents.push(PkgBase {
                            pkgbase: (*pkg).to_string(),
                            packages: vec![],
                        });
                    }
                    [pkg, child] => {
                        let parent = pkglist
                            .contents
                            .iter_mut()
                            .find(|p| p.pkgbase == *pkg)
                            .ok_or_else(|| GetPkglistError::MissingParent((*pkg).to_string()))?;

                        parent.packages.push(PackageReference {
                            name: (*child).to_string(),
                            repo: url.clone(),
                            pacscript: format!("{url}/packages/{pkg}/{pkg}.pacscript").parse()?,
                        });
                    }
                    _ => return Err(GetPkglistError::InvalidLine(pkg_entry.to_string())),
                }
            }
        }

        Ok(pkglist)
    }
}

impl IntoIterator for PkgList {
    type Item = PkgBase;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.contents.into_iter()
    }
}

impl PkgList {
    /// Filter out packages by query.
    #[must_use]
    pub fn filter_pkg<'a>(&'a self, search: &'a str) -> FilterPkg<'a> {
        FilterPkg {
            search,
            pkgs: self.contents.as_slice(),
        }
    }
}

impl IntoIterator for PkgBase {
    type Item = PackageReference;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.packages.into_iter()
    }
}

impl PkgBase {
    /// Does the package not have any child packages?
    #[must_use]
    pub fn is_single(&self) -> bool {
        self.packages.len() == 1 && self.pkgbase == self.packages[0].name
    }
}

impl FilterPkg<'_> {
    #[must_use]
    pub fn entries(&self) -> Vec<PkgDisplayEntry> {
        let mut out = vec![];

        for pkgbase in self.pkgs {
            let pkg_pkgbase = pkgbase.pkgbase.as_str();

            let mut matches: Vec<_> = pkgbase
                .packages
                .iter()
                .filter(|pkg| pkg_pkgbase.contains(self.search) || pkg.name.contains(self.search))
                .collect();

            if matches.is_empty() {
                continue;
            }

            matches.sort_by(|a, b| a.name.cmp(&b.name));

            if !pkgbase.is_single() {
                let source = metalink(&pkgbase.packages[0].repo)
                    .map_or_else(|| pkgbase.packages[0].pacscript.to_string(), |o| o.pretty());

                out.push(PkgDisplayEntry {
                    pkgbase: None,
                    name: format!("{}:pkgbase", pkgbase.pkgbase),
                    source: (source, pkgbase.packages[0].pacscript.to_string()),
                });
            }

            for pkg in matches {
                let source =
                    metalink(&pkg.repo).map_or_else(|| pkg.pacscript.to_string(), |o| o.pretty());

                out.push(PkgDisplayEntry {
                    pkgbase: if pkgbase.is_single() {
                        None
                    } else {
                        Some(pkg_pkgbase.to_string())
                    },
                    name: pkg.name.clone(),
                    source: (source, pkg.pacscript.to_string()),
                });
            }
        }

        out.sort_by(|a, b| a.name.cmp(&b.name));

        out
    }
}

impl Default for PackageColors {
    fn default() -> Self {
        Self {
            pkgbase: Color::Green,
            package: Color::Green,
            at_sign: Color::Magenta,
            source: Color::Cyan,
        }
    }
}

impl Display for PkgDisplayEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.color(&PackageColors::default()))
    }
}

impl PkgDisplayEntry {
    /// Display package with given colors.
    #[must_use]
    pub fn color(&self, color: &PackageColors) -> ColoredString {
        if let Some(self_pkgbase) = &self.pkgbase {
            format!(
                "{}{}{} {} {}",
                self_pkgbase.color(color.pkgbase),
                ":".color(color.pkgbase),
                self.name.color(color.package),
                "@".color(color.at_sign),
                vte_format!(self.source.1, "{}", self.source.0.color(color.source))
            )
            .into()
        } else {
            format!(
                "{} {} {}",
                self.name.color(color.package),
                "@".color(color.at_sign),
                vte_format!(self.source.1, "{}", self.source.0.color(color.source))
            )
            .into()
        }
    }
}
