use libpacstall::repo::repo::PacstallRepo;

fn main() {
    let repos = match PacstallRepo::open("/usr/share/pacstall/repo/pacstallrepo") {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };

    let custom_repos = PacstallRepo::try_from(vec!["https://pacstall.dev/ @baaa"]).unwrap();

    println!("{:#?}", custom_repos);
    println!("{}", custom_repos);
}
