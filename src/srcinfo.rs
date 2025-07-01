// Created originally by <https://github.com/D-Brox>.

use nom::{
    IResult, Parser,
    bytes::complete::{tag, take_until, take_until1, take_while1},
    character::complete::multispace0,
    combinator::opt,
    multi::{many0, separated_list0},
    sequence::preceded,
};

use crate::pkg::keys::{Arch, DistroClamp, Maintainer, Priority};

/// A representation of an `.SRCINFO` file.
#[derive(Debug, Default)]
pub struct SrcInfo {
    pub pkgbase: PkgBase,
    pub packages: Vec<PkgInfo>,
}

/// Keys paired to a global package.
#[derive(Debug, Default, Clone)]
pub struct PkgBase {
    pub pkgbase: String,
    pub pkgver: String,
    pub pkgrel: usize,
    pub epoch: usize,
    pub mask: Vec<String>,
    pub compatible: Vec<DistroClamp>,
    pub incompatible: Vec<DistroClamp>,
    pub maintainer: Vec<Maintainer>,
    pub source: Vec<(ArchDistro, String)>,
    pub noextract: Vec<String>,
    pub nosubmodules: Vec<String>,
    pub md5sums: Vec<(ArchDistro, String)>,
    pub sha1sums: Vec<(ArchDistro, String)>,
    pub sha224sums: Vec<(ArchDistro, String)>,
    pub sha256sums: Vec<(ArchDistro, String)>,
    pub sha384sums: Vec<(ArchDistro, String)>,
    pub sha512sums: Vec<(ArchDistro, String)>,
    pub b2sums: Vec<(ArchDistro, String)>,
    pub makedepends: Vec<(ArchDistro, String)>,
    pub makeconflicts: Vec<(ArchDistro, String)>,
}

/// An architecture and a distro, usually paired in a tuple with a key.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct ArchDistro {
    pub arch: Option<Arch>,
    pub distro: Option<String>,
}

/// Keys paired with a specific package.
#[derive(Debug, Default, Clone)]
pub struct PkgInfo {
    pub pkgname: String,
    pub pkgdesc: String,
    pub url: String,
    pub priority: Priority,
    pub arch: Vec<Arch>,
    pub license: Vec<String>,

    pub gives: Vec<(ArchDistro, String)>,
    pub depends: Vec<(ArchDistro, String)>,
    pub checkdepends: Vec<(ArchDistro, String)>,
    pub optdepends: Vec<(ArchDistro, String)>,
    pub checkconflicts: Vec<(ArchDistro, String)>,
    pub conflicts: Vec<(ArchDistro, String)>,
    pub provides: Vec<(ArchDistro, String)>,
    pub breaks: Vec<(ArchDistro, String)>,
    pub replaces: Vec<(ArchDistro, String)>,
    pub enhances: Vec<(ArchDistro, String)>,
    pub recommends: Vec<(ArchDistro, String)>,
    pub suggests: Vec<(ArchDistro, String)>,

    pub backup: Vec<String>,
    pub repology: Vec<String>,
}

impl SrcInfo {
    /// Construct a definitive package version.
    #[must_use]
    pub fn version(&self) -> String {
        let base = &self.pkgbase;
        if base.epoch != 0 {
            format!("{}:{}-{}", base.epoch, base.pkgver, base.pkgrel)
        } else {
            format!("{}-{}", base.pkgver, base.pkgrel)
        }
    }

    /// How many children packages are there?
    pub fn len(&self) -> usize {
        self.packages.len()
    }

    /// Parse a string into an [`SrcInfo`].
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (input, pairs) = many0(parse_key_value).parse(input)?;
        let mut srcinfo = Self::default();
        let mut global = PkgInfo::default();

        let mut current_pkg: Option<PkgInfo> = None;
        macro_rules! set {
            ($field:ident = $expr:expr) => {
                srcinfo.pkgbase.$field = $expr
            };
            ($field:ident + $expr:expr) => {
                srcinfo.pkgbase.$field.push($expr)
            };
            ($field:ident &= $expr:expr) => {
                if let Some(ref mut pkg) = current_pkg {
                    pkg.$field = $expr;
                } else {
                    global.$field = $expr;
                }
            };
            ($field:ident &+ $expr:expr) => {
                if let Some(ref mut pkg) = current_pkg {
                    pkg.$field.push($expr);
                } else {
                    global.$field.push($expr);
                }
            };
        }

        for (key, value) in pairs {
            let (base_key, arch_distro) = split_key_arch(&global.arch, &key);

            match base_key.as_str() {
                "pkgbase" => set!(pkgbase = value),
                "pkgver" => set!(pkgver = value),
                "pkgrel" => {
                    set!(pkgrel = value.parse::<usize>().expect("Could not convert to usize"))
                }
                "epoch" => {
                    set!(epoch = value.parse::<usize>().expect("Could not convert to usize"))
                }
                "mask" => set!(mask + value),
                "compatible" => {
                    set!(compatible + value.parse().expect("Could not convert to distroclamp"))
                }
                "incompatible" => {
                    set!(incompatible + value.parse().expect("Could not convert to distroclamp"))
                }
                "maintainer" => {
                    set!(maintainer + value.parse().expect("Could not convert to maintainer"))
                }
                "noextract" => set!(noextract + value),
                "nosubmodules" => set!(nosubmodules + value),

                "source" => set!(source + (arch_distro, value)),
                "md5sums" => set!(md5sums + (arch_distro, value)),
                "sha1sums" => set!(sha1sums + (arch_distro, value)),
                "sha224sums" => set!(sha224sums + (arch_distro, value)),
                "sha256sums" => set!(sha256sums + (arch_distro, value)),
                "sha384sums" => set!(sha384sums + (arch_distro, value)),
                "sha512sums" => set!(sha512sums + (arch_distro, value)),
                "b2sums" => set!(b2sums + (arch_distro, value)),
                "makedepends" => set!(makedepends + (arch_distro, value)),
                "makeconflicts" => set!(makeconflicts + (arch_distro, value)),

                "pkgname" => {
                    if let Some(pkg) = current_pkg.take() {
                        srcinfo.packages.push(pkg);
                    }
                    current_pkg = Some(global.clone());
                    set!(pkgname &= value);
                }

                "pkgdesc" => set!(pkgdesc &= value),
                "url" => set!(url &= value),
                "priority" => set!(priority &= value.into()),
                "arch" => set!(arch &+ value.into()),
                "license" => set!(license &+ value),
                "backup" => set!(backup &+ value),
                "repology" => set!(repology &+ value),

                "gives" => set!(gives &+ (arch_distro,value)),
                "depends" => set!(depends &+ (arch_distro,value)),
                "checkdepends" => set!(checkdepends &+ (arch_distro,value)),
                "optdepends" => set!(optdepends &+ (arch_distro,value)),
                "checkconflicts" => set!(checkconflicts &+ (arch_distro,value)),
                "conflicts" => set!(conflicts &+ (arch_distro,value)),
                "provides" => set!(provides &+ (arch_distro,value)),
                "breaks" => set!(breaks &+ (arch_distro,value)),
                "replaces" => set!(replaces &+ (arch_distro,value)),
                "enhances" => set!(enhances &+ (arch_distro,value)),
                "recommends" => set!(recommends &+ (arch_distro,value)),
                "suggests" => set!(suggests &+ (arch_distro,value)),

                _ => {}
            }
        }

        if let Some(pkg) = current_pkg.take() {
            srcinfo.packages.push(pkg);
        }

        Ok((input, srcinfo))
    }
}

fn parse_comment(input: &str) -> IResult<&str, ()> {
    let (input, _) = (multispace0, preceded(tag("#"), take_until("\n"))).parse(input)?;
    Ok((input, ()))
}

fn parse_key_value(input: &str) -> IResult<&str, (String, String)> {
    let (input, _) = (opt(parse_comment), multispace0).parse(input)?;
    let (input, key) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(input)?;
    let (input, _) = (multispace0, tag("="), multispace0).parse(input)?;
    let (input, value) = take_until1("\n")(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, (key.to_string(), value.trim().to_string())))
}

fn split_key_arch(arches: &[Arch], key: &str) -> (String, ArchDistro) {
    let split_key: Vec<_> = key.split('_').collect();
    match split_key.len() {
        2 => (
            split_key[0].to_string(),
            if arches.contains(&split_key[1].to_string().into()) {
                ArchDistro {
                    arch: Some(split_key[1].to_string().into()),
                    ..Default::default()
                }
            } else {
                ArchDistro {
                    distro: Some(split_key[1].to_string()),
                    ..Default::default()
                }
            },
        ),
        3 => (
            split_key[0].to_string(),
            ArchDistro {
                arch: Some(split_key[1].to_string().into()),
                distro: Some(split_key[2].to_string()),
            },
        ),
        _ => (key.to_string(), ArchDistro::default()),
    }
}

/// Parse an `srclist` file.
///
/// The difference between an `.SRCINFO` file and `srclist` is that `srclist` is a collection of
/// `.SRCINFO` separated by `---`.
///
/// See <https://github.com/pacstall/pacstall-programs/blob/master/srclist>.
pub fn parse_srclist(input: &str) -> IResult<&str, Vec<SrcInfo>> {
    let (input, _) = (many0(parse_comment), multispace0, tag("---\n")).parse(input)?;
    separated_list0(tag("---\n"), SrcInfo::parse).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_full() {
        let input = r#"
### Auto-generated for pacstall-programs
### Truncated for test
---
pkgbase = 1password-cli-bin
	gives = 1password-cli
	pkgver = 2.30.3
	pkgdesc = 1Password CLI
	arch = amd64
	maintainer = Oren Klopfer <oren@taumoda.com>
	repology = project: 1password-cli
	source = @1password-cli-bin~2.30.3::https://cache.agilebits.com/dist/1P/op2/pkg/v2.30.3/op_linux_amd64_v2.30.3.zip
	sha256sums = a16307ebcecb40fd091d7a6ff4f0c380c3c0897c4f4616de2c5d285e57d5ee28

pkgname = 1password-cli-bin
---
pkgbase = 86box-app
	gives = 86box
	pkgver = 4.2.1
	pkgdesc = Open source x86 emulator
	url = https://86box.net
	arch = amd64
	maintainer = James Ed Randson <jimedrand@disroot.org>
	source = https://github.com/86Box/86Box/releases/download/v4.2.1/86Box-Linux-x86_64-b6130.AppImage
	sha256sums = e049a364bc50307f7db9703960de7b6b1e3d35a7804800e98ce34ff9ba447e53

pkgname = 86box-app
---
pkgbase = abdownloadmanager
	pkgver = 1.5.3
	pkgdesc = Desktop app which lets you manage and organize your download files better than before
	arch = amd64
	license = Apache-2.0
	maintainer = villamorrd <villamorrd@students.nu-moa.edu.ph>
	repology = project: abdownloadmanager
	source = https://github.com/amir1376/ab-download-manager/releases/download/v1.5.3/ABDownloadManager_1.5.3_linux_x64.tar.gz
	sha256sums = ab43c3010cd820ad50f7604c89b5dc75ea9e6a9fa6455416514e77403715931f

pkgname = abdownloadmanager
    "#;
        let (_, srclist) = parse_srclist(input).expect("Should parse successfully");
        assert_eq!(srclist.len(), 3);
    }

    #[test]
    fn test_parse_source_archs() {
        let input = r#"
# test
pkgbase = mypkg
pkgver = 1.0
pkgrel = 1

arch = amd64
arch = aarch64
license = GPL

source = commonfile.tar.gz
source_amd64 = specialfile-x86_64.tar.gz
source_aarch64 = specialfile-aarch64.tar.gz

pkgname = mypkg-core
depends = openssl
"#;
        let (_, parsed) = SrcInfo::parse(input).expect("Should parse successfully");
        println!("{parsed:?}");

        assert_eq!(parsed.pkgbase.pkgbase, "mypkg");
        assert!(
            parsed
                .packages
                .iter()
                .all(|p| p.arch == vec![Arch::Amd64, Arch::Aarch64])
        );
        assert_eq!(
            parsed.pkgbase.source.first().unwrap(),
            &(ArchDistro::default(), "commonfile.tar.gz".to_string())
        );
        assert_eq!(
            parsed.pkgbase.source.get(1).unwrap(),
            &(
                ArchDistro {
                    arch: Some(Arch::Amd64),
                    distro: None
                },
                "specialfile-x86_64.tar.gz".to_string()
            )
        );
        assert_eq!(
            parsed.pkgbase.source.get(2).unwrap(),
            &(
                ArchDistro {
                    arch: Some(Arch::Aarch64),
                    distro: None
                },
                "specialfile-aarch64.tar.gz".to_string()
            )
        );
        assert_eq!(parsed.packages.len(), 1);
        assert_eq!(parsed.packages[0].pkgname, "mypkg-core");
    }
}
