use std::env;

use brush_core::ShellValue;
use sysinfo::System;

use crate::pkg::keys::{Arch, DistroClamp};

/// Generates variable names and values that pacscripts may use.
pub struct PacstallVariables<'a> {
    vars: Vec<(&'a str, String)>,
    sysinfo: System,
}

/// Converts values into [`ShellValue`]s.
pub(crate) trait AsShellValue {
    /// Convert a value into a [`ShellValue`].
    fn to_shellvalue(self) -> ShellValue;
}

/// Strings can be converted into [`ShellValue::String`].
impl<T> AsShellValue for T
where
    T: ToString,
{
    fn to_shellvalue(self) -> ShellValue {
        ShellValue::String(self.to_string())
    }
}

impl<'a> IntoIterator for PacstallVariables<'a> {
    /// The name of the variable and the value of the variable.
    ///
    /// As a contrived example:
    ///
    /// ```stdout
    /// ("NCPU", "8")
    /// ```
    type Item = (&'a str, String);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.vars.into_iter()
    }
}

impl PacstallVariables<'_> {
    /// Create a new set of variables.
    pub fn generate() -> Self {
        let sysinfo = System::new();
        Self {
            vars: vec![
                ("NCPU", Self::ncpu(&sysinfo).to_string()),
                ("CARCH", Self::carch().to_string()),
                ("DISTRO", Self::distro().to_string()),
                ("KVER", Self::kver()),
            ],
            sysinfo,
        }
    }

    /// Get number of CPUs or fetch from `$PACSTALL_BUILD_CORES`.
    #[must_use]
    fn ncpu(sysinfo: &System) -> usize {
        let cpu_count = sysinfo.cpus().len();
        if let Ok(cpu) = env::var("PACSTALL_BUILD_CORES") {
            cpu.parse::<usize>().unwrap_or(cpu_count)
        } else {
            cpu_count
        }
    }

    /// Get system architecture.
    #[must_use]
    fn carch() -> Arch<'static> {
        Arch::host()
    }

    /// Get current distro.
    #[must_use]
    fn distro() -> DistroClamp {
        DistroClamp::system().expect("OOPSIES")
    }

    /// Get current distro.
    #[must_use]
    fn kver() -> String {
        System::kernel_version().unwrap_or("<unknown>".to_owned())
    }
}
