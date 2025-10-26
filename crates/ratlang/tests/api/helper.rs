use std::fs;
use std::path::Path;

use ratlang::{Compiler, ExecutionConfig, SourceMap, Value, Vm};

pub fn run_source(name: &str, source: &str) -> Value {
    run_source_with_config(name, source, ExecutionConfig::default())
}

pub fn run_source_with_config(name: &str, source: &str, config: ExecutionConfig) -> Value {
    let mut map = SourceMap::new();
    map.add(name.to_string(), source.to_string());
    run_map(map, config)
}

pub fn run_file(path: &Path) -> Value {
    let contents = fs::read_to_string(path).expect("failed to read source file");
    run_source(path.to_string_lossy().as_ref(), &contents)
}

fn run_map(map: SourceMap, config: ExecutionConfig) -> Value {
    let compiler = Compiler::new();
    let compilation = compiler.compile(&map).expect("failed to compile source");
    let mut vm = Vm::new();
    vm.execute(&compilation, config)
        .expect("failed to execute program")
}
