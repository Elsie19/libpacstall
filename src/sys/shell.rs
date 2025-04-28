/// Generate a shell ready for sourcing pacscripts.
///
/// # Notes
/// This will include things such as all variables declared in
/// <https://github.com/pacstall/pacstall/wiki/101.1-Variables#built-in-variables>
/// and `ask()` and [`fancy_message()`](`crate::sys::builtins::FancyMessage`).
#[macro_export]
macro_rules! pacstall_shell {
    ($version:expr) => {{
        let options = ::brush_core::CreateOptions {
            shell_name: Some(String::from("pacstall_interpreter")),
            shell_product_display_str: Some(format!("pacstall {}", $version)),
            ..Default::default()
        };
        let mut shell = ::brush_core::Shell::new(&options).await?;

        // TODO: Replace these with register_builtin and add_env_var later.
        shell.builtins.insert(
            "fancy_message".into(),
            ::brush_core::builtins::simple_builtin::<$crate::sys::builtins::FancyMessage>(),
        );

        shell.env.set_global(
            "NCPU",
            ::brush_core::ShellVariable::new(::brush_core::ShellValue::String(
                $crate::sys::vars::ncpu().to_string(),
            )),
        )?;

        shell
    }};
}
