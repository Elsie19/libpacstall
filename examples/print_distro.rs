use libpacstall::pkg::keys::DistroClamp;

fn main() {
    let my_own_distro = DistroClamp::system().expect("Oof");
    println!("{}", my_own_distro);
}
