use anyhow::Result;
use clap::Parser;
use ratlang::{docs, source::SourceMap};

#[derive(Parser, Debug)]
#[command(
    name = "ratdoc",
    version,
    about = "Ratlang documentation generator (preview)"
)]
struct Args {
    /// Ratlang source file to document
    #[arg(value_name = "FILE")]
    input: String,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let source = std::fs::read_to_string(&args.input)?;
    let map = SourceMap::single(&args.input, source);
    match docs::generate(&map) {
        Ok(bundle) => {
            println!("{}", bundle.markdown);
        }
        Err(err) => eprintln!("documentation generation failed: {err}"),
    }
    Ok(())
}
