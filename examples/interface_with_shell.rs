use std::error::Error;

use brush_core::{CreateOptions, ExecutionParameters, Shell, builtins::simple_builtin};
use libpacstall::sys::builtins::FancyMessage;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let text = r#"fancy_message info "Hello: %s\n" "bar""#;
    let options = CreateOptions {
        shell_name: Some("pacstall_interpreter".into()),
        shell_product_display_str: Some(format!("pacstall {}", env!("CARGO_PKG_VERSION"))),
        ..Default::default()
    };
    let mut shell = Shell::new(&options).await?;
    shell
        .builtins
        .insert("fancy_message".into(), simple_builtin::<FancyMessage>());

    println!("{}", text);

    shell
        .run_string(text, &ExecutionParameters::default())
        .await?;

    Ok(())
}
