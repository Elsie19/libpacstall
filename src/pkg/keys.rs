use core::fmt;
use std::{
    borrow::Borrow,
    cmp::Ordering,
    convert::Infallible,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
    path::PathBuf,
    str::FromStr,
    sync::OnceLock,
};

use debversion::Version;
use etc_os_release::OsRelease;
use serde::Deserialize;
use strum_macros::EnumIter;
use thiserror::Error;
use url::Url;

use crate::srcinfo::ArchDistro;

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
    pub info: Option<Vec<DistroCSV>>,
}

/// Deserialization for `/usr/share/distro-info/*.csv`.
#[derive(Debug, Clone, Deserialize)]
pub struct DistroCSV {
    pub version: Option<String>,
    pub codename: String,
    pub series: String,
    pub created: String,
    pub release: String,
    pub eol: String,
    #[serde(alias = "eol-lts", alias = "eol-server", default)]
    pub eol_server: Option<String>,
    #[serde(alias = "eol-esm", alias = "eol-elts", default)]
    pub eol_esm: Option<String>,
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
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq, Clone, Hash, EnumIter)]
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
    Other(String),
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
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Maintainer {
    name: String,
    email: String,
}

#[derive(Debug, Error)]
pub enum MaintainerParseError {
    #[error("could not split string: `{0}`")]
    NoSplit(String),
}

/// Source entry schema.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct SourceEntry<'a> {
    /// Destination (`dest::`).
    pub dest: Option<PathBuf>,
    /// Extract to location (`@to_location::`).
    pub to_location: Option<PathBuf>,
    /// Target.
    pub source: SourceURLType<'a>,
}

/// Type of [`SourceEntry`] URL.
#[derive(PartialEq, Eq, Hash, Clone)]
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
#[derive(Default, PartialEq, Eq, Clone, Copy, Hash)]
pub enum GitTarget<'a> {
    /// Clone from branch.
    Branch(&'a str),
    /// Clone from tag.
    Tag(&'a str),
    /// Clone from commit.
    Commit(&'a str),
    /// Clone from `HEAD`.
    #[default]
    HEAD,
}

/// Hash sums.
///
/// # Notes
///
/// This will not attempt to verify that the given sums list is compatible with the given [`HashSumType`].
/// The caller should verify this before instantiation.
#[derive(PartialEq, Eq, Clone, Hash)]
pub struct HashSums {
    hasher: HashSumType,
    // The reason why this is `Option<String>` is because we need to represent `SKIP` somehow and we
    // can't just skip it in the vec.
    sums: Vec<Option<String>>,
}

/// Type of sum used.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
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
#[derive(PartialEq, Eq, Clone, Copy, Hash)]
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

/// Priority in packages.
///
/// See [priority](https://www.debian.org/doc/debian-policy/ch-archive.html#s-priorities).
#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Priority {
    Essential,
    /// Packages which are necessary for the proper functioning of the system.
    Required,
    /// Important programs, including those which one would expect to find on any Unix-like system.
    Important,
    /// These packages provide a reasonably small but not too limited character-mode system.
    Standard,
    /// This is the default priority for the majority of packages.
    #[default]
    Optional,
}

/// Wrapper for an `optdepend` key.
#[derive(Default, Debug, PartialEq, Eq, Clone, Hash)]
pub struct OptDescription {
    pkg: String,
    desc: Option<String>,
}

/// Type of package in [`PackageString`].
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PackageKind {
    Source,
    Deb,
    Git,
    AppImage,
    Binary,
}

/// Pattern-matchable package type.
///
/// This can and should be used exactly like a [`String`] in 99% of cases. You should never have to
/// deal with the variants inside, and if that happens, it should be reported as a bug.
#[derive(Eq, Clone)]
#[non_exhaustive]
pub enum PackageString {
    /// A source package that has no suffix. Usually builds from source tarball.
    Source(String, OnceLock<String>),
    /// A deb package.
    Deb(String, OnceLock<String>),
    /// A rolling package with no locked version.
    Git(String, OnceLock<String>),
    /// An appimage.
    AppImage(String, OnceLock<String>),
    /// A binary package.
    Binary(String, OnceLock<String>),
}

impl Debug for PackageString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(
            match self {
                PackageString::Source(s, _)
                | PackageString::Deb(s, _)
                | PackageString::Git(s, _)
                | PackageString::AppImage(s, _)
                | PackageString::Binary(s, _) => s,
            },
            f,
        )
    }
}

impl HashSumType {
    pub const fn size(&self) -> usize {
        match self {
            HashSumType::B2 | HashSumType::Sha512 => 128,
            HashSumType::Sha384 => 96,
            HashSumType::Sha256 => 64,
            HashSumType::Sha224 => 56,
            HashSumType::Sha1 => 40,
            HashSumType::Md5 => 32,
        }
    }
}

impl Display for PackageKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PackageKind::Source => "",
                PackageKind::Deb => "deb",
                PackageKind::Git => "git",
                PackageKind::AppImage => "app",
                PackageKind::Binary => "bin",
            }
        )
    }
}

impl Default for PackageString {
    fn default() -> Self {
        Self::Source(String::default(), OnceLock::default())
    }
}

impl Display for PackageString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for PackageString {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for (suffix, variant) in [
            ("-deb", PackageString::deb as fn(String) -> PackageString),
            ("-git", PackageString::git),
            ("-app", PackageString::appimage),
            ("-bin", PackageString::binary),
        ] {
            if let Some(name) = s.strip_suffix(suffix) {
                return Ok(variant(name.to_string()));
            }
        }

        Ok(PackageString::source(s.to_string()))
    }
}

impl<T: Into<String>> From<T> for PackageString {
    fn from(value: T) -> Self {
        Self::from_str(&value.into()).expect("Infallable conversion failed")
    }
}

impl PackageString {
    fn lock() -> OnceLock<String> {
        OnceLock::new()
    }

    fn source(s: String) -> Self {
        Self::Source(s, Self::lock())
    }

    fn deb(s: String) -> Self {
        Self::Deb(s, Self::lock())
    }

    fn git(s: String) -> Self {
        Self::Git(s, Self::lock())
    }

    fn appimage(s: String) -> Self {
        Self::AppImage(s, Self::lock())
    }

    fn binary(s: String) -> Self {
        Self::Binary(s, Self::lock())
    }

    /// Get package name as a string.
    pub fn as_str(&self) -> &str {
        match self {
            Self::Source(s, cache) => cache.get_or_init(|| s.clone()),
            Self::Deb(s, cache) => cache.get_or_init(|| format!("{s}-deb")),
            Self::Git(s, cache) => cache.get_or_init(|| format!("{s}-git")),
            Self::AppImage(s, cache) => cache.get_or_init(|| format!("{s}-app")),
            Self::Binary(s, cache) => cache.get_or_init(|| format!("{s}-bin")),
        }
    }

    /// Split string into component parts.
    ///
    /// # Examples
    /// ```
    /// # use libpacstall::pkg::keys::{PackageString, PackageKind};
    /// let my_pkg = "neofetch-git".parse::<PackageString>()?;
    /// assert_eq!(my_pkg.split(), ("neofetch", PackageKind::Git));
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn split(&self) -> (&str, PackageKind) {
        match self {
            PackageString::Source(s, _) => (s, PackageKind::Source),
            PackageString::Deb(s, _) => (s, PackageKind::Deb),
            PackageString::Git(s, _) => (s, PackageKind::Git),
            PackageString::AppImage(s, _) => (s, PackageKind::AppImage),
            PackageString::Binary(s, _) => (s, PackageKind::Binary),
        }
    }
}

impl AsRef<str> for PackageString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Deref for PackageString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl PartialEq<str> for PackageString {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<T: AsRef<str>> PartialEq<T> for PackageString {
    fn eq(&self, other: &T) -> bool {
        self.as_str() == other.as_ref()
    }
}

impl PartialEq<PackageString> for str {
    fn eq(&self, other: &PackageString) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<PackageString> for String {
    fn eq(&self, other: &PackageString) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Display for OptDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            if let Some(desc) = &self.desc {
                format!("{}: {desc}", self.pkg)
            } else {
                self.pkg.to_string()
            }
        )
    }
}

impl From<String> for OptDescription {
    fn from(value: String) -> Self {
        if let Some(index) = value.rfind(':') {
            let pkg = value[..index].trim().to_string();
            let desc = value[index + 1..].trim();
            let desc = if desc.is_empty() {
                None
            } else {
                Some(desc.to_string())
            };
            Self { pkg, desc }
        } else {
            Self {
                pkg: value.trim().to_string(),
                desc: None,
            }
        }
    }
}

impl Display for Priority {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Priority::Essential => "essential",
                Priority::Required => "required",
                Priority::Important => "important",
                Priority::Standard => "standard",
                Priority::Optional => "optional",
            }
        )
    }
}

impl From<String> for Priority {
    fn from(value: String) -> Self {
        #[allow(clippy::wildcard_in_or_patterns)]
        match value.to_lowercase().as_str() {
            "essential" => Self::Essential,
            "required" => Self::Required,
            "important" => Self::Important,
            "standard" => Self::Standard,
            "optional" | _ => Self::Optional,
        }
    }
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
    pub const fn new(cmp: Option<VerCmp>, version: Version) -> Self {
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

    /// Get hashsums as a vector.
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

impl<'a, I> ArchIterator<'a> for I
where
    I: Iterator<Item = &'a Arch>,
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

impl From<String> for Arch {
    fn from(value: String) -> Self {
        match value.to_lowercase().as_str() {
            "any" => Arch::Any,
            "all" => Arch::All,
            "amd64" => Arch::Amd64,
            "arm64" => Arch::Arm64,
            "armel" => Arch::Armel,
            "armhf" => Arch::Armhf,
            "i386" => Arch::I386,
            "ppc64el" => Arch::Ppc64el,
            "riscv64" => Arch::Riscv64,
            "s390x" => Arch::S390x,
            "x86_64" => Arch::X86_64,
            "aarch64" => Arch::Aarch64,
            "arm" => Arch::Arm,
            "armv7h" => Arch::Armv7h,
            "i686" => Arch::I686,
            other => Arch::Other(other.to_string()),
        }
    }
}

impl Display for Arch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Arch::Any => "any",
                Arch::All => "all",
                Arch::Amd64 => "amd64",
                Arch::Arm64 => "arm64",
                Arch::Armel => "armel",
                Arch::Armhf => "armhf",
                Arch::I386 => "i386",
                Arch::Ppc64el => "ppc64el",
                Arch::Riscv64 => "riscv64",
                Arch::S390x => "s390x",
                Arch::X86_64 => "x86_64",
                Arch::Aarch64 => "aarch64",
                Arch::Arm => "arm",
                Arch::Armv7h => "armv7h",
                Arch::I686 => "i686",
                Arch::Other(other) => other,
            }
        )
    }
}

/// See [`Arch::compatible`] for more information.
impl<T: Borrow<ArchDistro>> PartialEq<T> for Arch {
    fn eq(&self, other: &T) -> bool {
        match other.borrow().arch {
            Some(ref other_arch) => self.compatible(other_arch),
            None => true,
        }
    }
}

impl Arch {
    /// Check if an [`Arch`] is an Arch Linux style architecture or a Debian style.
    #[must_use]
    pub const fn is_arch_style(&self) -> bool {
        matches!(
            self,
            Self::X86_64 | Self::Aarch64 | Self::Arm | Self::Armv7h | Self::I686
        )
    }

    /// Get default architecture from system.
    #[must_use]
    pub fn host() -> Self {
        match std::env::consts::ARCH {
            "x86" => Self::I386,
            "x86_64" => Self::Amd64,
            "arm" => Self::Armel,
            "aarch64" => Self::Arm64,
            "powerpc64" => Self::Ppc64el,
            "riscv64" => Self::Riscv64,
            "s390x" => Self::S390x,
            default => Self::Other(default.to_owned()),
        }
    }

    /// Is this architecture compatible with another?
    pub fn compatible(&self, other: &Self) -> bool {
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

impl Maintainer {
    /// Make new maintainer key.
    ///
    /// # Examples
    ///
    /// ```
    /// # use libpacstall::pkg::keys::Maintainer;
    /// let my_maintainer = Maintainer::new("Elsie", "hwengerstickel@pm.me");
    /// ```
    #[must_use]
    pub fn new<S: Into<String>>(name: S, email: S) -> Self {
        Self {
            name: name.into(),
            email: email.into(),
        }
    }

    /// Get name from [`Maintainer`].
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get email from [`Maintainer`].
    #[must_use]
    pub fn email(&self) -> &str {
        &self.email
    }
}

impl FromStr for Maintainer {
    type Err = MaintainerParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once('<') {
            Some((first, last)) => {
                let first = first.trim();

                let mut last = last.chars();
                last.next_back();
                let email = last.as_str();

                Ok(Self::new(first, email))
            }
            None => Err(MaintainerParseError::NoSplit(s.to_string())),
        }
    }
}

/// The display output conforms (*generally*) to the `name-addr` portion of
/// <https://www.rfc-editor.org/rfc/rfc5322#section-3.4>.
///
/// Its expected to look like `$name <$email>`.
impl Display for Maintainer {
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

impl<T: Borrow<ArchDistro>> PartialEq<T> for DistroClamp {
    fn eq(&self, other: &T) -> bool {
        let other = other.borrow();
        // ArchDistro's distro is Option<String>, so get string or "*" if None
        let other_distro = other.distro.as_deref().unwrap_or("*");
        // ArchDistro has no explicit version, so reuse distro as version fallback
        let other_version = other.distro.as_deref().unwrap_or("*");

        if self.basic_match(other_distro, other_version) {
            true
        } else {
            self.nuanced_match(other_distro, other_version)
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
        let other_distro = other.distro.as_str();
        let other_version = other.version.as_str();

        if self.basic_match(other_distro, other_version) {
            true
        } else {
            self.nuanced_match(other_distro, other_version)
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

    /// Check the structural equality of two clamps.
    pub fn lit_eq(&self, other: &Self) -> bool {
        self.distro == other.distro && self.version == other.version
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

    /// Basic match of distro and version strings, with wildcards.
    fn basic_match(&self, other_distro: &str, other_version: &str) -> bool {
        if self.distro == other_distro {
            match (self.version.as_str(), other_version) {
                (_, "*") | ("*", _) => true,
                (ver, other_ver) => ver == other_ver,
            }
        } else {
            match (self.distro.as_str(), other_distro) {
                (_, "*") | ("*", _) => true,
                (ver, other_ver) => ver == other_ver,
            }
        }
    }

    /// The nuanced check with extra info, accepting distro/version strings.
    fn nuanced_match(&self, other_distro: &str, other_version: &str) -> bool {
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

                if other_distro == "*" {
                    other_version == extra.distro_version_number
                        || other_version == extra.distro_version_name
                        || other_version == extra.distro_parent_vname.clone().unwrap_or_default()
                } else if other_version == "*" {
                    other_distro == extra.distro_name
                        || Some(other_distro.to_string()) == extra.distro_parent
                } else {
                    self_clamp
                        == &DistroClamp::new(extra.distro_name.clone(), extra.distro_version_name)
                            .unwrap()
                        || self_clamp
                            == &DistroClamp::new(extra.distro_name, extra.distro_version_number)
                                .unwrap()
                        || self_clamp
                            == &DistroClamp::new(
                                extra.distro_parent.clone().unwrap_or_default(),
                                extra.distro_parent_vname.clone().unwrap_or_default(),
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
            _ => false,
        }
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
    fn package_type() {
        let my_pkg = "hello-app".parse::<PackageString>().unwrap();
        assert_eq!(my_pkg, "hello-app");
        assert!(matches!(my_pkg, PackageString::AppImage(..)));
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
