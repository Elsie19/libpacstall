use libpacstall::pkg::keys::DistroClamp;

fn main() {
    let my_distro = DistroClamp::system().expect("Dang");
    let other_distro = DistroClamp::new("ubuntu", "24.04").unwrap();
    assert!(my_distro == other_distro)
}
