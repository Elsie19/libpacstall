use core::fmt;
use std::{cmp::Ordering, fmt::Display, path::PathBuf, str::FromStr};

use debversion::Version;
use thiserror::Error;
use url::Url;

/// Distro clamping, useful in `incompatible`/`compatible`.
///
/// # Examples
///
/// ```
/// # use libpacstall::pkg::keys::DistroClamp;
/// let any_16_04: DistroClamp = "*:16.04".parse()?;
/// assert_eq!(any_16_04.version(), "16.04");
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct DistroClamp {
    distro: String,
    version: String,
}

/// Errors from parsing [`DistroClamp`].
#[derive(Debug, Error)]
pub enum DistroClampError {
    /// Missing `:` betwen distro and version.
    #[error("missing `:`")]
    NoSplit,
    /// Double glob `*:*`.
    #[error("double glob found")]
    DoubleGlob,
}

/// Package architectures.
#[derive(Debug, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Arch {
    /// Package can be compiled on *any* system, but will only run on the compiled architecture.
    Any,
    /// Package can run on *all* systems, regardless of architecture.
    All,
    Amd64,
    Arm64,
    Armel,
    Armhf,
    I386,
    Mips64el,
    Ppc64el,
    Riscv64,
    S390x,
    // Arch types
    X86_64,
    Aarch64,
    Arm,
    Armv7h,
    I686,
    // Other
    Other(String),
}

/// Maintainer schema.
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Maintainer {
    name: String,
    email: String,
}

/// Source entry schema.
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct SourceEntry {
    /// Destination (`dest::`).
    pub dest: Option<PathBuf>,
    /// Extract to location (`@to_location::`).
    pub to_location: Option<PathBuf>,
    /// Target.
    pub source: SourceURLType,
}

/// Type of [`SourceEntry`] URL.
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SourceURLType {
    /// Git URL.
    Git {
        /// URL of git repository.
        url: Url,
        /// What to clone.
        target: GitTarget,
    },
    /// File URL.
    File(Url),
    /// URL.
    Url(Url),
}

/// What part of repository to clone.
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum GitTarget {
    /// Clone from branch.
    Branch(String),
    /// Clone from tag.
    Tag(String),
    /// Clone from commit.
    Commit(String),
    /// Clone from `HEAD`.
    HEAD,
}

/// Hash sums.
///
/// # Notes
/// This will not attempt to verify that the given sums list is compatible with the given [`HashSumType`].
/// The caller should verify this before instantiation.
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct HashSums {
    hasher: HashSumType,
    // The reason why this is `Option<String>` is because we need to represent `SKIP` somehow and we
    // can't just skip it in the vec.
    sums: Vec<Option<String>>,
}

/// Type of sum used.
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum HashSumType {
    Sha256,
    Sha512,
    Sha384,
    Sha224,
    Sha1,
    Md5,
    B2,
}

/// Package version with an expected range clamp.
#[derive(Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct VersionClamp {
    cmp: Option<VerCmp>,
    version: Version,
}

/// Version comparisons for [`Version`].
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum VerCmp {
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
}

impl PartialOrd for VersionClamp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.version.partial_cmp(&other.version)
    }
}

impl PartialEq for VersionClamp {
    fn eq(&self, other: &Self) -> bool {
        if self.cmp.is_none() {
            true
        } else {
            self.cmp == other.cmp && self.version == other.version
        }
    }
}

impl Display for VerCmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Eq => "=",
                Self::Lt => "<",
                Self::Gt => ">",
                Self::Le => "<=",
                Self::Ge => ">=",
            }
        )
    }
}

impl Display for VersionClamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(cmp) = &self.cmp {
            write!(f, "{}{}", cmp, self.version)
        } else {
            write!(f, "{}", self.version)
        }
    }
}

impl VersionClamp {
    /// Make new [`VersionClamp`].
    ///
    /// A [`Option::None`] value for `cmp` indicates that any version provided can satisfy the
    /// package.
    pub fn new(cmp: Option<VerCmp>, version: Version) -> Self {
        Self { cmp, version }
    }

    /// Check if this version can be satisfied by another version.
    ///
    /// Generally, if this succeeds, you shouldn't have to worry about the [`Ordering`] return
    /// value, but if this fails, the [`Ordering`] value may be useful in error messages.
    #[must_use]
    pub fn satisfied_by(&self, other: &Self) -> (bool, Ordering) {
        let order = self
            .version
            .partial_cmp(&other.version)
            .expect("Should be impossible for a comparison to fail");
        match &self.cmp {
            Some(cmp) => match cmp {
                VerCmp::Eq => (self.version == other.version, order),
                VerCmp::Lt => (self.version < other.version, order),
                VerCmp::Gt => (self.version > other.version, order),
                VerCmp::Le => (self.version <= other.version, order),
                VerCmp::Ge => (self.version >= other.version, order),
            },
            None => (true, Ordering::Equal),
        }
    }
}

impl IntoIterator for HashSums {
    type Item = Option<String>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.sums.into_iter()
    }
}

impl fmt::Debug for HashSums {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.sums.iter().map(|entr| match entr {
                Some(o) => o,
                None => "SKIP",
            }))
            .finish()
    }
}

impl<T> From<&[T]> for HashSums
where
    T: AsRef<str>,
{
    fn from(value: &[T]) -> Self {
        Self::new(value.iter().map(AsRef::as_ref), HashSumType::Sha256)
    }
}

impl<T> From<Vec<T>> for HashSums
where
    T: AsRef<str>,
{
    fn from(value: Vec<T>) -> Self {
        Self::new(value.iter().map(AsRef::as_ref), HashSumType::Sha256)
    }
}

impl HashSums {
    /// Create new hashsums.
    ///
    /// # Examples
    ///
    /// ```
    /// # use libpacstall::pkg::keys::{HashSums, HashSumType};
    /// let my_sums_str = vec![
    ///     "SKIP",
    ///     "0231d51a2cd76b8b8557a6d35a5b64658542788f55a73b6b1f48d5942ff9bb16",
    ///     "SKIP",
    ///     "914ab68d1ea6e2182897961468c59c942c77308e75c69743c026bfe40b6b2de1",
    /// ];
    /// let my_sums = HashSums::new(my_sums_str, HashSumType::Sha256);
    /// assert_eq!(
    ///     &my_sums.as_vec(),
    ///     &[
    ///         None,
    ///         Some("0231d51a2cd76b8b8557a6d35a5b64658542788f55a73b6b1f48d5942ff9bb16"),
    ///         None,
    ///         Some("914ab68d1ea6e2182897961468c59c942c77308e75c69743c026bfe40b6b2de1")
    ///     ]
    /// );
    /// ```
    pub fn new<I, T>(contents: I, hasher: HashSumType) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<String>,
    {
        Self {
            hasher,
            sums: contents
                .into_iter()
                .map(|entr| match entr.into().as_ref() {
                    "SKIP" => None,
                    default => Some(default.into()),
                })
                .collect(),
        }
    }

    #[must_use]
    pub fn as_vec(&self) -> Vec<Option<&str>> {
        self.sums.iter().map(Option::as_deref).collect::<Vec<_>>()
    }
}

impl GitTarget {
    pub fn branch<S: Into<String>>(branch: S) -> Self {
        Self::Branch(branch.into())
    }

    pub fn tag<S: Into<String>>(tag: S) -> Self {
        Self::Tag(tag.into())
    }

    pub fn commit<S: Into<String>>(commit: S) -> Self {
        Self::Commit(commit.into())
    }
}

impl Display for GitTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Branch(branch) => write!(f, "#branch={branch}"),
            Self::Tag(tag) => write!(f, "#tag={tag}"),
            Self::Commit(commit) => write!(f, "#commit={commit}"),
            Self::HEAD => write!(f, ""),
        }
    }
}

impl Display for SourceURLType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::File(file) | Self::Url(file) => write!(f, "{file}"),
            Self::Git { url, .. } => {
                if url.path().ends_with(".git") {
                    write!(f, "{url}")
                } else {
                    write!(f, "git+{url}")
                }
            }
        }
    }
}

impl Default for GitTarget {
    fn default() -> Self {
        Self::HEAD
    }
}

impl PartialEq for Arch {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Amd64, Self::X86_64) | (Self::X86_64, Self::Amd64) => true,
            (Self::Arm64, Self::Aarch64) | (Self::Aarch64, Self::Arm64) => true,
            (Self::Armel, Self::Arm) | (Self::Arm, Self::Armel) => true,
            (Self::Armhf, Self::Armv7h) | (Self::Armv7h, Self::Armhf) => true,
            (Self::I386, Self::I686) | (Self::I686, Self::I386) => true,
            _ => self == other,
        }
    }
}

impl Maintainer {
    /// Make new maintainer key.
    ///
    /// # Examples
    ///
    /// ```
    /// # use libpacstall::pkg::keys::Maintainer;
    /// let my_maintainer = Maintainer::new("Elsie", "hwengerstickel@pm.me");
    /// ```
    pub fn new<S: Into<String>>(name: S, email: S) -> Self {
        Self {
            name: name.into(),
            email: email.into(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn email(&self) -> &str {
        &self.email
    }
}

impl Display for Maintainer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <{}>", self.name, self.email)
    }
}

impl Display for SourceEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dest = self.dest.as_ref().map(|d| d.to_string_lossy());
        let to_location = self.to_location.as_ref().map(|l| l.to_string_lossy());

        let source_str = match &self.source {
            matched @ SourceURLType::Git { .. } => format!("{matched}"),
            _ => self.source.to_string(),
        };

        match (dest, to_location) {
            (Some(dest), Some(loc)) => write!(f, "{dest}@{loc}::{source_str}"),
            (Some(dest), None) => write!(f, "{dest}::{source_str}"),
            (None, Some(loc)) => write!(f, "@{loc}::{source_str}"),
            (None, None) => write!(f, "{source_str}"),
        }
    }
}

impl Display for DistroClamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.distro, self.version)
    }
}

impl FromStr for DistroClamp {
    type Err = DistroClampError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(':') {
            Some((distro, version)) => {
                if distro == "*" && version == "*" {
                    Err(DistroClampError::DoubleGlob)
                } else {
                    Ok(Self {
                        distro: distro.to_string(),
                        version: version.to_string(),
                    })
                }
            }
            None => Err(DistroClampError::NoSplit),
        }
    }
}

/// The rules are as such:
///
/// * `*:ver` == `any:ver`
/// * `any:*` == `any:ver`
/// * `any:ver` == `any:ver`
impl PartialEq for DistroClamp {
    fn eq(&self, other: &Self) -> bool {
        let distro_match = self.distro == "*" || other.distro == "*" || self.distro == other.distro;
        let version_match =
            self.version == "*" || other.version == "*" || self.version == other.version;
        distro_match && version_match
    }
}

impl DistroClamp {
    #[must_use]
    pub fn distro(&self) -> &str {
        &self.distro
    }

    #[must_use]
    pub fn version(&self) -> &str {
        &self.version
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn distroclamp_version_glob() {
        let first: DistroClamp = "ubuntu:*".parse().unwrap();
        let second: DistroClamp = "ubuntu:16.04".parse().unwrap();
        assert_eq!(first, second);
    }

    #[test]
    fn distroclamp_distro_glob() {
        let first: DistroClamp = "*:16.04".parse().unwrap();
        let second: DistroClamp = "ubuntu:16.04".parse().unwrap();
        assert_eq!(first, second);
    }

    #[test]
    #[should_panic]
    fn distroclamp_double_glob() {
        "*:*".parse::<DistroClamp>().unwrap();
    }

    #[test]
    #[should_panic]
    fn distroclamp_no_colon() {
        "*".parse::<DistroClamp>().unwrap();
    }

    #[test]
    fn vec_to_sums() {
        let my_sums_str = vec![
            "SKIP",
            "0231d51a2cd76b8b8557a6d35a5b64658542788f55a73b6b1f48d5942ff9bb16",
            "SKIP",
            "914ab68d1ea6e2182897961468c59c942c77308e75c69743c026bfe40b6b2de1",
        ];
        let my_sums = HashSums::new(my_sums_str, HashSumType::Sha256);
        assert_eq!(
            &my_sums.as_vec(),
            &[
                None,
                Some("0231d51a2cd76b8b8557a6d35a5b64658542788f55a73b6b1f48d5942ff9bb16"),
                None,
                Some("914ab68d1ea6e2182897961468c59c942c77308e75c69743c026bfe40b6b2de1")
            ]
        );
    }

    #[test]
    fn ver_compare() {
        let first_version = VersionClamp::new(Some(VerCmp::Le), "1.2.4".parse().unwrap());
        let second_version = VersionClamp::new(None, "1.2.5".parse().unwrap());
        assert!(first_version < second_version);
        assert!(first_version.satisfied_by(&second_version).0);
        let third_version = VersionClamp::new(None, "1.0.0".parse().unwrap());
        assert!(!first_version.satisfied_by(&third_version).0);
    }
}
