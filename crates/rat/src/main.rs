use anyhow::Result;
use ratlang::{eval, format};
use rustyline::{error::ReadlineError, DefaultEditor};

fn main() -> Result<()> {
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
                    println!("Commands:\n  :quit/:exit  leave the REPL\n  :help        show this message\n  :fmt <code>  format snippet");
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
