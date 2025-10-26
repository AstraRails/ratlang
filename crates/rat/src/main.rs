use std::path::PathBuf;

use anyhow::{bail, Context, Result};
use clap::Parser;
use ratlang::{eval, format, Compiler, ExecutionConfig, SourceMap, Value, Vm};
use rustyline::{error::ReadlineError, DefaultEditor};

#[derive(Parser, Debug)]
#[command(name = "rat", version, about = "Ratlang CLI (AI-authored preview)")]
struct Cli {
    /// Path to a Ratlang source file. When omitted, launches the interactive REPL.
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,
    /// Arguments passed to main(args) inside the Ratlang program (requires FILE).
    #[arg(value_name = "ARGS", trailing_var_arg = true)]
    script_args: Vec<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.file.is_none() && !cli.script_args.is_empty() {
        bail!("script arguments require a file path");
    }

    if let Some(path) = cli.file {
        run_script(path, cli.script_args)
    } else {
        run_repl()
    }
}

fn run_repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    println!("Ratlang REPL (preview) — type :quit to exit");
    loop {
        match rl.readline("rat> ") {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.eq(":quit") || trimmed.eq(":exit") {
                    break;
                }
                if trimmed.eq(":help") {
                    println!(
                        "Commands:\n  :quit/:exit  leave the REPL\n  :help        show this message\n  :fmt <code>  format snippet"
                    );
                    continue;
                }
                if let Some(rest) = trimmed.strip_prefix(":fmt ") {
                    match format(rest) {
                        Ok(formatted) => println!("{formatted}"),
                        Err(err) => eprintln!("format error: {err}"),
                    }
                    continue;
                }
                match eval(trimmed) {
                    Ok(value) => println!("{value:?}"),
                    Err(err) => eprintln!("error: {err}"),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("repl error: {err}");
                break;
            }
        }
    }
    Ok(())
}

fn run_script(path: PathBuf, args: Vec<String>) -> Result<()> {
    let display = path.to_string_lossy().into_owned();
    let source = std::fs::read_to_string(&path)
        .with_context(|| format!("failed to read Ratlang source at {display}"))?;
    let sources = SourceMap::single(display, source);

    let compiler = Compiler::new();
    let compilation = compiler.compile(&sources)?;

    let mut vm = Vm::new();
    let mut config = ExecutionConfig::default();
    config.args = args;

    let value = vm.execute(&compilation, config)?;
    handle_exit_value(value);
    Ok(())
}

fn handle_exit_value(value: Value) {
    match value {
        Value::Int(code) => {
            if code != 0 {
                eprintln!("program exited with code {code}");
                std::process::exit(code as i32);
            }
        }
        other => {
            println!("{other:?}");
        }
    }
}
