use std::io::Cursor;

use libpacstall::local::repos::PacstallRepos;

#[test]
fn read_and_print() {
    let data = r#"
    https://raw.githubusercontent.com/pacstall/pacstall-programs/master @pacstall
    file:///some/repo/
    "#
    .trim()
    .as_bytes();
    let cursor = Cursor::new(&data[..]);
    let repos = PacstallRepos::open(cursor).expect("Should have read");
    assert_eq!(repos[0].alias(), Some("pacstall"));
    assert_eq!(repos[1].alias(), None);
}
