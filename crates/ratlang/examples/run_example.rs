use ratlang::{compiler::Compiler, runtime::{ExecutionConfig, Vm}, source::SourceMap};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args().nth(1).unwrap_or_else(|| "examples/example.rat".to_string());
    let source = std::fs::read_to_string(&path)?;
    let sources = SourceMap::single(path.clone(), source);

    let compiler = Compiler::new();
    let compilation = compiler.compile(&sources)?;

    let mut vm = Vm::new();
    let value = vm.execute(&compilation, ExecutionConfig::default())?;
    println!("program returned: {value:?}");

    Ok(())
}
