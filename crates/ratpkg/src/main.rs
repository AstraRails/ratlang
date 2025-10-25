use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(name = "ratpkg", version, about = "Ratlang package manager (preview)")]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Initialise a new Ratlang package (not yet implemented)
    Init,
    /// Publish a package to the registry (not yet implemented)
    Publish,
}

fn main() -> Result<()> {
    let args = Args::parse();
    eprintln!("ratpkg is a preview stub. Command = {:?}", args.command);
    Ok(())
}
