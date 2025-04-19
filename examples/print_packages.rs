use clap::Parser;
use std::{
    collections::HashMap,
    fs::File,
    io::{Cursor, Read},
};

use libpacstall::local::repos::PacstallRepos;
use reqwest::blocking;

macro_rules! vte_format {
    ($link:expr, $($arg:tt)*) => {
        format!("\x1b]8;;{}\x1b\\{}\x1b]8;;\x1b\\", $link, format!($($arg)*))
    };
}

/// Vastly simplified version of `pacstall -S`.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Use default repository layout.
    ///
    /// Useful if you do not have pacstall installed.
    #[arg(short, long)]
    emulate: bool,

    /// Search term.
    keyword: String,
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();

    let reader: Box<dyn Read> = if args.emulate {
        let data = b"https://raw.githubusercontent.com/pacstall/pacstall-programs/master @pacstall";
        Box::new(Cursor::new(&data[..]))
    } else {
        Box::new(File::open("/usr/share/pacstall/repo/pacstallrepo")?)
    };

    let repos = PacstallRepos::open(reader).expect("Could not read");

    let mut pkgs: HashMap<String, PacstallRepos> = HashMap::new();

    for entry in repos {
        // For every entry we have in our repo list, get the packagelist.
        let body = match blocking::get(format!("{}/packagelist", entry.url())) {
            // Get the text of it.
            Ok(body) => body.text().expect("Could not unwrap text"),
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
        if pkg.0.contains(&args.keyword) {
            println!(
                "\x1b[32m{}\x1b[0m \x1b[35m@\x1b[0m \x1b[36m{}\x1b[0m",
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

    Ok(())
}
