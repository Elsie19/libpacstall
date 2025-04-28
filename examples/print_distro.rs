use libpacstall::pkg::keys::DistroClamp;

fn main() {
    let my_distro = DistroClamp::system().unwrap();
    let other = DistroClamp::new("ubuntu", "24.10").unwrap();
    assert!(my_distro == other)
}
