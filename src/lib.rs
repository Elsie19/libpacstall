//! Libpacstall is the underlying library used to interface with low level pacstall operations.

/// Handles local repository related operations.
///
/// Most of this modules functionality relates to parsing files in `/usr/share/pacstall/repo/`.
pub mod local;

/// Handles individual packages.
pub mod pkg;
