use anyhow::Result;
use clap::Parser;
use std::io::{self, Read};

#[derive(Parser, Debug)]
#[command(name = "ratfmt", version, about = "Ratlang formatter (preview)")]
struct Args {
    /// Optional file to format. Reads stdin when omitted.
    #[arg(value_name = "FILE")]
    input: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let mut source = String::new();
    if let Some(path) = args.input {
        source = std::fs::read_to_string(&path)?;
    } else {
        io::stdin().read_to_string(&mut source)?;
    }
    match ratlang::format(&source) {
        Ok(formatted) => print!("{formatted}"),
        Err(err) => {
            eprintln!("formatting failed: {err}");
        }
    }
    Ok(())
}
