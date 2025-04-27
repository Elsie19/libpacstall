use std::fmt::Display;
use std::io::Write;

use brush_core::builtins::{BuiltinResult, ExitCode, SimpleCommand};
use sprintf::{Printf, vsprintf};

/// `fancy_message` implementation.
pub struct FancyMessage;

#[derive(PartialEq, Eq)]
enum FancyMessageType {
    Info,
    Warn,
    Error,
    Sub,
    Unknown,
}

impl From<&str> for FancyMessageType {
    fn from(value: &str) -> Self {
        match value {
            "info" => Self::Info,
            "warn" => Self::Warn,
            "error" => Self::Error,
            "sub" => Self::Sub,
            _ => Self::Unknown,
        }
    }
}

impl Display for FancyMessageType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Info => "[\x1b[1;32m+\x1b[0m] \x1b[1mINFO\x1b[0m:",
                Self::Warn => "[\x1b[1;33m*\x1b[0m] WARNING\x1b[0m:",
                Self::Error => "[\x1b[1;31m!\x1b[0m] ERROR\x1b[0m:",
                Self::Sub => "\t[\x1b[1;34m>\x1b[0m]",
                Self::Unknown => "[\x1b[1m?\x1b[0m] UNKNOWN\x1b[0m:",
            }
        )
    }
}

impl SimpleCommand for FancyMessage {
    fn execute<I: Iterator<Item = S>, S: AsRef<str>>(
        context: brush_core::ExecutionContext<'_>,
        args: I,
    ) -> Result<brush_core::builtins::BuiltinResult, brush_core::Error> {
        let args: Vec<String> = args
            .into_iter()
            // Skip `$0`
            .skip(1)
            .map(|str| str.as_ref().to_string())
            .collect();
        match args.len() {
            0 => {
                writeln!(context.stderr(), "Expected style and message")?;
                Err(brush_core::Error::InvalidArguments)
            }
            1 => {
                if FancyMessageType::Unknown == args[0].as_str().into() {
                    writeln!(context.stderr(), "Expected message type")?;
                } else {
                    writeln!(context.stderr(), "Expected message")?;
                }
                Err(brush_core::Error::InvalidArguments)
            }
            2.. => {
                let output_type: FancyMessageType = args[0].as_str().into();
                let out = if args.len() == 2 {
                    &args[1]
                } else {
                    let vsprintf_args: Vec<&dyn Printf> =
                        args[2..].iter().map(|elem| elem as &dyn Printf).collect();
                    &vsprintf(&args[1], &vsprintf_args).unwrap()
                };
                match output_type {
                    FancyMessageType::Info | FancyMessageType::Sub => {
                        write!(context.stdout(), "{output_type} {out}")?;
                        context.stdout().flush()?;
                    }
                    _ => {
                        write!(context.stderr(), "{output_type} {out}")?;
                        context.stderr().flush()?;
                    }
                }
                Ok(BuiltinResult {
                    exit_code: ExitCode::Success,
                })
            }
        }
    }

    fn get_content(
        name: &str,
        _content_type: brush_core::builtins::ContentType,
    ) -> Result<String, brush_core::Error> {
        Ok(name.into())
    }
}
