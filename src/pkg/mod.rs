/// Package format.
pub mod package;

/// Individual components of a package.
///
/// Roughly, the following pacscript keys correspond to the struct:
///
/// | Key                         | Associated Item                                  | Notes                          |
/// |-----------------------------|--------------------------------------------------|--------------------------------|
/// | `incompatible`/`compatible` | [`DistroClamp`](`crate::pkg::keys::DistroClamp`) | Should be wrapped with [`Vec`] |
/// | `*sums`                     | [`HashSums`](`crate::pkg::keys::HashSums`)       |                                |
/// | `maintainer`                | [`Maintainer`](`crate::pkg::keys::Maintainer`)   | Should be wrapped with [`Vec`] |
/// | `source`                    | [`SourceEntry`](`crate::pkg::keys::SourceEntry`) | Should be wrapped with [`Vec`] |
pub mod keys;
