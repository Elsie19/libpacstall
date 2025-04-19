use std::{collections::HashMap, io::Cursor};

use libpacstall::local::repos::PacstallRepos;
use reqwest::blocking;

macro_rules! vte_format {
    ($link:expr, $($arg:tt)*) => {
        format!("\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\", $link, format!($($arg)*))
    };
}

#[test]
fn print_packages() {
    let data = r#"
    https://raw.githubusercontent.com/pacstall/pacstall-programs/master @pacstall
    "#
    .trim()
    .as_bytes();
    let cursor = Cursor::new(&data[..]);
    let repos = PacstallRepos::open(cursor).expect("Could not read");

    let keyword = "neo";

    let mut pkgs: HashMap<String, PacstallRepos> = HashMap::new();

    for entry in repos {
        let body = match blocking::get(format!("{}/packagelist", entry.url())) {
            Ok(body) => body.text().expect("Could not unwrap text").to_string(),
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };
        for pkg in body.lines() {
            pkgs.entry(pkg.to_string())
                .and_modify(|p| p.push(entry.clone()))
                .or_insert(entry.clone().into());
        }
    }

    for pkg in pkgs {
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
