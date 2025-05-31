use core::fmt;
use std::{cmp::Ordering, fmt::Display, path::PathBuf, str::FromStr};

use debversion::Version;
use etc_os_release::OsRelease;
use serde::Deserialize;
use thiserror::Error;
use url::Url;

/// Distro clamping, useful in `incompatible`/`compatible`.
///
/// # Examples
///
/// ```
/// # use libpacstall::pkg::keys::DistroClamp;
/// let any_16_04 = "*:16.04".parse::<DistroClamp>()?;
/// assert_eq!(any_16_04.version(), "16.04");
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(Debug, Clone)]
pub struct DistroClamp {
    distro: String,
    version: String,
    os_release: Option<OsRelease>,
    info: Option<Vec<DistroCSV>>,
}

/// Deserialization for `/usr/share/distro-info/*.csv`.
#[derive(Debug, Clone, Deserialize)]
struct DistroCSV {
    version: Option<String>,
    codename: String,
    series: String,
    created: String,
    release: String,
    eol: String,
    #[serde(alias = "eol-lts", alias = "eol-server", default)]
    eol_server: Option<String>,
    #[serde(alias = "eol-esm", alias = "eol-elts", default)]
    eol_esm: Option<String>,
}

struct DistroClampExtra {
    distro_name: String,
    distro_version_name: String,
    distro_version_number: String,
    distro_pretty_name: String,
    distro_parent: Option<String>,
    distro_parent_vname: Option<String>,
    distro_parent_number: Option<String>,
}

/// Errors from parsing [`DistroClamp`].
#[derive(Debug, Error)]
pub enum DistroClampError {
    /// Missing `:` betwen distro and version.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use libpacstall::pkg::keys::DistroClamp;
    /// let clamp = "ubuntu16.04".parse::<DistroClamp>().unwrap(); // <- Will fail because there is no `:`.
    /// ```
    #[error("missing `:`")]
    NoSplit,
    /// Double glob `*:*`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use libpacstall::pkg::keys::DistroClamp;
    /// let clamp = "*:*".parse::<DistroClamp>().unwrap(); // <- Will fail because there is a double glob, which is disallowed.
    /// ```
    #[error("double glob found")]
    DoubleGlob,
    #[error("os release error")]
    OsReleaseError(#[from] etc_os_release::Error),
    #[error("version could not be found from os-release")]
    EmptyVersion,
    #[error("error in distro info")]
    DistroInfo(#[from] csv::Error),
}

/// Package architectures.
///
/// # Examples
///
/// Checking if list has homogeneous architecture styles.
///
/// ```
/// # use libpacstall::pkg::keys::Arch;
/// use libpacstall::pkg::keys::ArchIterator; // Needed for `.allowed()`.
///
/// let my_mixed_archs = vec![Arch::Amd64, Arch::X86_64];
/// assert!(!my_mixed_archs.iter().allowed());
///
/// let my_same_archs = vec![Arch::Amd64, Arch::I386];
/// assert!(my_same_archs.iter().allowed());
/// ```
#[derive(Debug, Eq, Clone, Copy)]
pub enum Arch<'a> {
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
    /// Custom architecture not known by pacstall.
    Other(&'a str),
}

/// Adds [`ArchIterator::allowed`] to any iterator that iterates over [`Arch`].
pub trait ArchIterator<'a> {
    /// Checks that the current value is allowed to co-exist with another architecture value.
    ///
    /// # Notes
    ///
    /// Pacstall does not allow mix-and-matching Arch style and Debian style architectures, hence
    /// the existence of this trait and method.
    fn allowed(self) -> bool;
}

/// Maintainer schema.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Maintainer<'a> {
    name: &'a str,
    email: &'a str,
}

/// Source entry schema.
#[derive(PartialEq, Eq, Clone)]
pub struct SourceEntry<'a> {
    /// Destination (`dest::`).
    pub dest: Option<PathBuf>,
    /// Extract to location (`@to_location::`).
    pub to_location: Option<PathBuf>,
    /// Target.
    pub source: SourceURLType<'a>,
}

/// Type of [`SourceEntry`] URL.
#[derive(PartialEq, Eq, Clone)]
pub enum SourceURLType<'a> {
    /// Git URL.
    Git {
        /// URL of git repository.
        url: Url,
        /// What to clone.
        target: GitTarget<'a>,
    },
    /// File URL.
    File(Url),
    /// URL.
    Url(Url),
}

/// What part of repository to clone.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum GitTarget<'a> {
    /// Clone from branch.
    Branch(&'a str),
    /// Clone from tag.
    Tag(&'a str),
    /// Clone from commit.
    Commit(&'a str),
    /// Clone from `HEAD`.
    HEAD,
}

/// Hash sums.
///
/// # Notes
///
/// This will not attempt to verify that the given sums list is compatible with the given [`HashSumType`].
/// The caller should verify this before instantiation.
#[derive(PartialEq, Eq, Clone)]
pub struct HashSums {
    hasher: HashSumType,
    // The reason why this is `Option<String>` is because we need to represent `SKIP` somehow and we
    // can't just skip it in the vec.
    sums: Vec<Option<String>>,
}

/// Type of sum used.
#[derive(PartialEq, Eq, Clone, Copy)]
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
#[derive(Eq, Clone)]
pub struct VersionClamp {
    cmp: Option<VerCmp>,
    version: Version,
}

/// Version comparisons for [`Version`].
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum VerCmp {
    /// Equal (`==`).
    Eq,
    /// Less-than (`<`).
    Lt,
    /// Greater-than (`>`).
    Gt,
    /// Less-or-equal-than (`<=`).
    Le,
    /// Greater-or-equal-than (`>=`).
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
    /// An [`Option::None`] value for `cmp` indicates that any version provided can satisfy the
    /// package, generally used for when you want "just the package".
    #[must_use]
    pub fn new(cmp: Option<VerCmp>, version: Version) -> Self {
        Self { cmp, version }
    }

    /// Check if this version can be satisfied by another version.
    ///
    /// Generally, if this succeeds, you shouldn't have to worry about the [`Ordering`] return
    /// value, but if this fails, the [`Ordering`] value may be useful in error messages.
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
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

impl Display for GitTarget<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Branch(branch) => write!(f, "#branch={branch}"),
            Self::Tag(tag) => write!(f, "#tag={tag}"),
            Self::Commit(commit) => write!(f, "#commit={commit}"),
            Self::HEAD => write!(f, ""),
        }
    }
}

impl Display for SourceURLType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::File(file) | Self::Url(file) => write!(f, "{file}"),
            Self::Git { url, .. } => {
                // This isn't a "file extension".
                #[allow(clippy::case_sensitive_file_extension_comparisons)]
                if url.path().ends_with(".git") {
                    write!(f, "{url}")
                } else {
                    write!(f, "git+{url}")
                }
            }
        }
    }
}

impl Default for GitTarget<'_> {
    fn default() -> Self {
        Self::HEAD
    }
}

impl<'a, I> ArchIterator<'a> for I
where
    I: Iterator<Item = &'a Arch<'a>>,
{
    fn allowed(self) -> bool {
        let mut peekable_iter = self.peekable();

        match peekable_iter.next() {
            Some(first) => {
                peekable_iter.all(|arch_item| arch_item.is_arch_style() == first.is_arch_style())
            }
            None => true,
        }
    }
}

impl Arch<'_> {
    /// Check if an [`Arch`] is an Arch Linux style architecture or a Debian style.
    #[must_use]
    pub fn is_arch_style(&self) -> bool {
        matches!(
            self,
            Self::X86_64 | Self::Aarch64 | Self::Arm | Self::Armv7h | Self::I686
        )
    }

    /// Get default architecture from system.
    #[must_use]
    pub fn system_arch() -> Self {
        match std::env::consts::ARCH {
            "x86" => Self::I386,
            "x86_64" => Self::Amd64,
            "arm" => Self::Armel,
            "aarch64" => Self::Arm64,
            "mips64" | "mips64r6" => Self::Mips64el,
            "powerpc64" => Self::Ppc64el,
            "riscv64" => Self::Riscv64,
            "s390x" => Self::S390x,
            default => Self::Other(default),
        }
    }
}

impl PartialEq for Arch<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Arm64, Self::Aarch64)
            | (Self::Aarch64, Self::Arm64)
            | (Self::Armel, Self::Arm)
            | (Self::Arm, Self::Armel)
            | (Self::Amd64, Self::X86_64)
            | (Self::Armv7h, Self::Armhf)
            | (Self::Armhf, Self::Armv7h)
            | (Self::X86_64, Self::Amd64)
            | (Self::I386, Self::I686)
            | (Self::I686, Self::I386) => true,
            _ => self == other,
        }
    }
}

impl<'a> Maintainer<'a> {
    /// Make new maintainer key.
    ///
    /// # Examples
    ///
    /// ```
    /// # use libpacstall::pkg::keys::Maintainer;
    /// let my_maintainer = Maintainer::new("Elsie", "hwengerstickel@pm.me");
    /// ```
    #[must_use]
    pub fn new(name: &'a str, email: &'a str) -> Self {
        Self { name, email }
    }

    /// Get name from [`Maintainer`].
    #[must_use]
    pub fn name(&self) -> &str {
        self.name
    }

    /// Get email from [`Maintainer`].
    #[must_use]
    pub fn email(&self) -> &str {
        self.email
    }
}

/// The display output conforms (*generally*) to the `name-addr` portion of
/// <https://www.rfc-editor.org/rfc/rfc5322#section-3.4>.
///
/// Its expected to look like `$name <$email>`.
impl Display for Maintainer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <{}>", self.name, self.email)
    }
}

impl Display for SourceEntry<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dest = self.dest.as_ref().map(|d| d.to_string_lossy());
        let to_location = self.to_location.as_ref().map(|l| l.to_string_lossy());

        let source_str = match &self.source {
            matched @ SourceURLType::Git { .. } => matched.to_string(),
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
            Some((distro, version)) => Ok(Self::new(distro, version)?),
            None => Err(DistroClampError::NoSplit),
        }
    }
}

/// The rules are as such:
///
/// * `*:ver` == `any:ver`
/// * `any:*` == `any:ver`
/// * `any:ver` == `any:ver`
///
/// There is also a special case for when this check has more information (given by
/// [`DistroClamp::system`]).
impl PartialEq for DistroClamp {
    fn eq(&self, other: &Self) -> bool {
        let basic_match = if self.distro == other.distro {
            match (self.version.as_str(), other.version.as_str()) {
                (_, "*") | ("*", _) => true,
                (ver, other_ver) => ver == other_ver,
            }
        } else {
            match (self.distro.as_str(), other.distro.as_str()) {
                (_, "*") | ("*", _) => true,
                (ver, other_ver) => ver == other_ver,
            }
        };

        if basic_match {
            true
        } else {
            // These are the nuanced checks.
            match (&self.os_release, &self.info) {
                (Some(os), Some(info)) => {
                    let extra = Self::generate_extra(os, info);

                    let self_clamp = if let Some(ref parent) = extra.distro_parent {
                        &DistroClamp::new(
                            parent,
                            &extra
                                .distro_parent_vname
                                .clone()
                                .unwrap_or_else(|| extra.distro_version_name.to_string()),
                        )
                        .unwrap()
                    } else {
                        self
                    };

                    if other.distro == "*" {
                        other.version == extra.distro_version_number
                            || other.version == extra.distro_version_name
                            || other.version == extra.distro_parent_vname.unwrap_or_default()
                    } else if other.version == "*" {
                        other.distro == extra.distro_name
                            || Some(other.distro.clone()) == extra.distro_parent
                    } else {
                        self_clamp
                            == &DistroClamp::new(
                                extra.distro_name.clone(),
                                extra.distro_version_name,
                            )
                            .unwrap()
                            || self_clamp
                                == &DistroClamp::new(extra.distro_name, extra.distro_version_number)
                                    .unwrap()
                            || self_clamp
                                == &DistroClamp::new(
                                    extra.distro_parent.clone().unwrap_or_default(),
                                    extra.distro_parent_vname.unwrap_or_default(),
                                )
                                .unwrap()
                            || self_clamp
                                == &DistroClamp::new(
                                    extra.distro_parent.unwrap_or_default(),
                                    extra.distro_parent_number.unwrap_or_default(),
                                )
                                .unwrap()
                    }
                }
                _ => basic_match,
            }
        }
    }
}

impl DistroClamp {
    /// Create new [`DistroClamp`] from a distro and version.
    ///
    /// # Note
    ///
    /// This should be used sparingly! The system's clamp should be created from
    /// [`DistroClamp::system`], and most incoming clamps will be in string format and will need to
    /// be `.parse()`ed.
    ///
    /// # Errors
    ///
    /// Will error if both `distro` and `version` are globs.
    ///
    /// # Examples
    ///
    /// ```
    /// # use libpacstall::pkg::keys::DistroClamp;
    /// let distro = DistroClamp::new("ubuntu", "24.04").expect("Could not parse");
    /// ```
    pub fn new<S: Into<String>>(distro: S, version: S) -> Result<Self, DistroClampError> {
        let distro = distro.into();
        let version = version.into();
        if distro == "*" && version == "*" {
            Err(DistroClampError::DoubleGlob)
        } else {
            Ok(Self {
                distro,
                version,
                os_release: None,
                info: None,
            })
        }
    }

    #[must_use]
    pub fn distro(&self) -> &str {
        &self.distro
    }

    #[must_use]
    pub fn version(&self) -> &str {
        &self.version
    }

    /// Get running system's [`DistroClamp`].
    ///
    /// This method should always be used to base checking
    /// other clamps against, because it comes with extra context that it can match more
    /// abstractly.
    ///
    /// # Errors
    ///
    /// Will fail if `/usr/share/distro-info/(ubuntu ? debian).csv` does not exist, or if the file
    /// cannot be deserialized.
    pub fn system() -> Result<Self, DistroClampError> {
        let os_release = OsRelease::open()?;
        Ok(Self {
            distro: os_release.id().to_string(),
            version: os_release
                .version_codename()
                .ok_or(DistroClampError::EmptyVersion)?
                .to_string(),
            os_release: Some(os_release.clone()),
            info: Some(if os_release.id() == "ubuntu" {
                csv::ReaderBuilder::new()
                    .flexible(true)
                    .from_path("/usr/share/distro-info/ubuntu.csv")?
                    .into_deserialize()
                    .collect::<Result<Vec<DistroCSV>, csv::Error>>()?
            } else {
                csv::ReaderBuilder::new()
                    .flexible(true)
                    .from_path("/usr/share/distro-info/debian.csv")?
                    .into_deserialize()
                    .collect::<Result<Vec<DistroCSV>, csv::Error>>()?
            }),
        })
    }

    fn generate_extra(os: &OsRelease, info: &[DistroCSV]) -> DistroClampExtra {
        let distro_name = os.id();
        let mut distro_version_name = os.version_codename().unwrap_or_default();
        let mut distro_version_number = os.version_id().unwrap_or_default();
        let distro_pretty_name = os.pretty_name();

        let mut distro_parent = None;
        let mut distro_parent_vname = None;
        let mut distro_parent_number = None;

        match (
            os.get_value("DEBIAN_CODENAME"),
            os.get_value("UBUNTU_CODENAME"),
        ) {
            (Some(codename), _) => {
                distro_parent = Some("debian");
                distro_parent_vname = Some(codename.to_owned());
            }
            (None, Some(codename)) => {
                distro_parent = Some("ubuntu");
                distro_parent_vname = Some(codename.to_owned());
            }
            _ => {}
        }

        match distro_name {
            "debian" => {
                if let Some(matched) = info
                    .iter()
                    .find(|entry| entry.series == distro_version_name)
                {
                    if let Some(ver) = &matched.version {
                        distro_version_number = ver;
                    }
                }
            }
            "devuan" => {
                distro_parent = Some("debian");

                if distro_version_name.ends_with(" ceres") {
                    let trimmed = distro_version_name
                        .split_whitespace()
                        .next()
                        .unwrap_or_default();
                    distro_version_name = trimmed;
                    distro_parent_vname = Some("sid".to_string());
                } else if let Ok(content) = std::fs::read_to_string("/etc/debian_version") {
                    let debian_version = content.trim().to_string();
                    if debian_version.contains('.') {
                        if let Some(matched) = info.iter().find(|entry| {
                            if let Some(ver) = &entry.version {
                                ver == debian_version.split('.').next().unwrap_or_default()
                            } else {
                                false
                            }
                        }) {
                            distro_parent_vname = Some(matched.series.to_string());
                        }
                    } else {
                        distro_parent_vname = Some(debian_version.to_string());
                    }
                }
            }
            _ => {}
        }

        if distro_pretty_name.ends_with("/sid") || distro_version_name == "kali-rolling" {
            distro_parent = Some("debian");
            distro_parent_vname = Some("sid".into());
        }

        if let Some(parent) = distro_parent {
            if parent == "ubuntu"
                && distro_version_name == distro_parent_vname.clone().unwrap_or_default()
            {
                distro_parent_vname = None;
            } else {
                if let Some(vname) = &distro_parent_vname {
                    if let Some(matched) = info.iter().find(|entry| entry.series == *vname) {
                        if let Some(ver) = &matched.version {
                            distro_parent_number = Some(ver.replace(" LTS", ""));
                        }
                    }
                }
                if distro_parent_vname.as_deref() == Some("sid") {
                    distro_parent_number = Some("sid".to_string());
                }
            }
        }

        DistroClampExtra {
            distro_name: distro_name.to_string(),
            distro_version_name: distro_version_name.to_string(),
            distro_version_number: distro_version_number.to_string(),
            distro_pretty_name: distro_pretty_name.to_string(),
            distro_parent: distro_parent.map(ToOwned::to_owned),
            distro_parent_vname: distro_parent_vname.as_deref().map(ToOwned::to_owned),
            distro_parent_number,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn distroclamp_version_glob() {
        let first = "ubuntu:*".parse::<DistroClamp>().unwrap();
        let second = "ubuntu:16.04".parse::<DistroClamp>().unwrap();
        assert_eq!(first, second);
    }

    #[test]
    fn distroclamp_distro_glob() {
        let first = "*:16.04".parse::<DistroClamp>().unwrap();
        let second = "ubuntu:16.04".parse::<DistroClamp>().unwrap();
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
