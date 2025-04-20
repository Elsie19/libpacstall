use std::fs::{File, OpenOptions};
use std::io::Write;

use clap::{ArgGroup, Parser};
use libpacstall::local::repos::{PacstallRepo, PacstallRepos};

/// Vastly simplified version of `pacstall -A`/`pacstall -Rr`.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None, group(ArgGroup::new("repo_action").required(true).args(&["add_repo", "remove_repo"])))]
struct Args {
    /// Add a given repository to pacstall.
    #[arg(short = 'A', long, conflicts_with = "remove_repo")]
    add_repo: Option<String>,

    /// Remove a given repository from pacstall.
    #[arg(short = 'R', long, conflicts_with = "add_repo")]
    remove_repo: Option<String>,
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();

    let repos = PacstallRepos::open(File::open("/usr/share/pacstall/repo/pacstallrepo")?)
        .expect("Could not read");

    match (args.add_repo, args.remove_repo) {
        (Some(add), None) => {
            println!("Adding repo");
        }
        (None, Some(remove)) => {
            let to_remove = match remove.parse::<PacstallRepo>() {
                Ok(o) => o,
                Err(e) => {
                    eprintln!("Could not parse for reason: {e}");
                    std::process::exit(1);
                }
            };

            let save = repos
                .into_iter()
                .filter(|repo| *repo != to_remove)
                .collect::<PacstallRepos>();

            let mut file = OpenOptions::new()
                .write(true)
                .truncate(true)
                .open("/usr/share/pacstall/repo/pacstallrepo")?;

            writeln!(file, "{}", save)?;
        }
        _ => unreachable!(),
    }

    Ok(())
}
