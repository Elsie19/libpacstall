use std::{collections::HashMap, io::Cursor};

use libpacstall::local::repos::PacstallRepos;
use reqwest::blocking;

macro_rules! vte_format {
    ($link:expr, $($arg:tt)*) => {
        format!("\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\", $link, format!($($arg)*))
    };
}

fn main() {
    // This is to get around having a file with this contents, we can simply "simulate" a file with
    // [`Cursor`].
    let data = r#"
    https://raw.githubusercontent.com/pacstall/pacstall-programs/master @pacstall
    "#
    .trim()
    .as_bytes();
    let cursor = Cursor::new(&data[..]);
    // Read our contents.
    let repos = PacstallRepos::open(cursor).expect("Could not read");

    let keyword = "neo";

    let mut pkgs: HashMap<String, PacstallRepos> = HashMap::new();

    for entry in repos {
        // For every entry we have in our repo list, get the packagelist.
        let body = match blocking::get(format!("{}/packagelist", entry.url())) {
            // Get the text of it.
            Ok(body) => body.text().expect("Could not unwrap text").to_string(),
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };

        // Loop over packagelist
        for pkg in body.lines() {
            // Open or create the hashmap entry with the package name.
            pkgs.entry(pkg.to_string())
                // If it exists already, push the PacstallRepo entry to it, linking a given package
                // to a new remote repo.
                .and_modify(|p| p.push(entry.clone()))
                // If it doesn't exist, create the PacstallRepos list.
                .or_insert(entry.clone().into());
        }
    }

    for pkg in pkgs {
        // The actual "search" part that checks if text matches.
        if pkg.0.contains(&keyword) {
            println!(
                "{} @ {}",
                pkg.0,
                vte_format!(
                    format!(
                        "{}/packages/{}/{}.pacscript",
                        pkg.1.first().unwrap().url().to_string(),
                        pkg.0,
                        pkg.0
                    ),
                    "{}",
                    pkg.1.first().unwrap().url().to_string()
                )
            );
        }
    }
}
