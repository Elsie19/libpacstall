#![doc(
    html_logo_url = "https://raw.githubusercontent.com/pacstall/website/refs/heads/master/client/public/android-chrome-192x192.png"
)]
#![doc(
    html_favicon_url = "https://raw.githubusercontent.com/pacstall/website/refs/heads/master/client/public/favicon-32x32.png"
)]
//! Libpacstall is the underlying library used to interface with low level pacstall operations.
//!
//! You may want to check out some [basic examples](https://github.com/Elsie19/libpacstall/tree/master/examples)
//! to get a better grasp or just look through the documention here.

/// Handles local repository related operations.
///
/// Most of this modules functionality relates to parsing files in `/usr/share/pacstall/repo/`.
pub mod local;

/// Handles individual packages.
pub mod pkg;
