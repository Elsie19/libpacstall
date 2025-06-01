use std::path::Path;

use brush_core::{
    CreateOptions, ExecutionParameters, ExecutionResult, Shell, ShellVariable,
    builtins::simple_builtin,
};

use super::{
    builtins::FancyMessage,
    vars::{AsShellValue, PacstallVariables},
};

/// Handle for the environment that packages are built in.
pub struct PacstallShell {
    pub shell: Shell,
}

impl PacstallShell {
    /// Creates a new shell from [`brush_core`] with pacstall related enhancements.
    pub async fn new(version: &str) -> Result<Self, brush_core::Error> {
        let options = CreateOptions {
            shell_name: Some("pacstall_interpreter".to_string()),
            shell_product_display_str: Some(format!("pacstall v{}", version)),
            ..Default::default()
        };

        let mut shell = Shell::new(&options).await?;

        shell.register_builtin("fancy_message", simple_builtin::<FancyMessage>());

        // TODO: Add `homedir` $ (misc/scripts/package-base.sh:238).
        // TODO: Add `pacfile` $ (misc/scripts/package-base.sh:244).
        // TODO: Add `CDISTRO` $ (misc/scripts/package-base.sh:253).
        for (name, output) in PacstallVariables::generate() {
            shell.set_env_global(name, ShellVariable::new(output.to_shellvalue()))?;
        }

        Ok(Self { shell })
    }

    // TODO: Add srcinfo.print_out $ (misc/scripts/package-base.sh:264).
    /// Source a pacscript into the environment.
    pub async fn load_pacscript<P: AsRef<Path>>(
        &mut self,
        pacscript: P,
    ) -> Result<ExecutionResult, brush_core::Error> {
        self.shell
            .source_script(
                pacscript.as_ref(),
                std::iter::empty::<&str>(),
                &ExecutionParameters::default(),
            )
            .await
    }

    pub fn has_variable(&self, var: &str) -> Option<&ShellVariable> {
        self.shell.get_env_var(var)
    }
}
