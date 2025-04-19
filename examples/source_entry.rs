use std::path::PathBuf;

use libpacstall::pkg::package::{GitTarget, SourceEntry, SourceURLType};
use url::Url;

fn main() {
    let source = SourceEntry {
        dest: Some(PathBuf::from("here")),
        to_location: None,
        source: SourceURLType::Git {
            url: Url::parse("https://github.com/Elsie19/fancy_message")
                .expect("Could not parse URL"),
            target: GitTarget::tag("1.0.0"),
        },
    };
    println!("{}", source);
}
