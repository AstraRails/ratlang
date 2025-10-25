use anyhow::Result;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "ratc", version, about = "Ratlang compiler (preview)")]
struct Args {
    /// Path to the Ratlang source file to compile
    #[arg(value_name = "FILE", default_value = "-")]
    input: String,

    /// Output path for the generated artifact
    #[arg(short, long, value_name = "FILE")]
    output: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    eprintln!(
        "ratc is not fully implemented yet. Received input={} output={:?}",
        args.input,
        args.output
    );
    Ok(())
}
