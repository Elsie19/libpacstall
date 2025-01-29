use std::collections::HashMap;

use libpacstall::repo::{
    repo::{PacstallRepo, RepoEntry},
    repo_type::PackageRepo,
};
use reqwest::blocking;

macro_rules! vte_format {
    ($link:expr, $($arg:tt)*) => {
        format!("\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\", $link, format!($($arg)*))
    };
}

fn main() {
    let repos = match PacstallRepo::open("/usr/share/pacstall/repo/pacstallrepo") {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };

    let Some(keyword) = std::env::args().nth(1) else {
        eprintln!("No arg passed in");
        std::process::exit(1);
    };

    let mut pkgs: HashMap<String, Vec<RepoEntry>> = HashMap::new();

    for entry in repos {
        let body = match blocking::get(format!("{}/packagelist", entry.url.raw())) {
            Ok(body) => body.text().expect("Could not unwrap text").to_string(),
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        };
        for pkg in body.lines() {
            pkgs.entry(pkg.to_string())
                .and_modify(|p| p.push(entry.clone()))
                .or_insert(vec![entry.clone()]);
        }
    }

    for pkg in pkgs {
        if pkg.0.contains(&keyword) {
            println!(
                "{} @ {}",
                pkg.0,
                vte_format!(
                    pkg.1
                        .first()
                        .expect("How did a package get in here without being in a repo")
                        .url
                        .with_path(format!("packages/{}/{}.pacscript", pkg.0, pkg.0)),
                    "{}",
                    pkg.1.first().unwrap().url.search()
                )
            );
        }
    }
}
