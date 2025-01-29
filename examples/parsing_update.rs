use libpacstall::repo::update::Update;

fn main() {
    let update = match Update::open("/usr/share/pacstall/repo/update") {
        Ok(o) => o,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    println!(
        "In the update file, it is pointed to the user `{}` and the branch `{}`",
        update.username, update.branch
    );
}
