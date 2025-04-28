use std::error::Error;

use brush_core::ExecutionParameters;
use libpacstall::pacstall_shell;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let text = r#"
        fancy_message info "Hello: %s\n" "bar"
        echo "I have ${NCPU} cores"
    "#;

    let mut shell = pacstall_shell!(env!("CARGO_PKG_VERSION"));

    shell
        .run_string(text, &ExecutionParameters::default())
        .await?;

    Ok(())
}
