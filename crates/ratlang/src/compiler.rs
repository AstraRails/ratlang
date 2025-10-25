use crate::ast::Program;
use crate::diagnostics::{RatError, RatResult};
use crate::lexer;
use crate::parser;
use crate::source::SourceMap;
use crate::typeck::{self, TypeInfo};

#[derive(Debug, Clone)]
pub struct CompilationOptions {
    pub allow_warnings: bool,
}

impl Default for CompilationOptions {
    fn default() -> Self {
        Self {
            allow_warnings: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Compilation {
    pub program: Program,
    pub type_info: TypeInfo,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    options: CompilationOptions,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            options: CompilationOptions::default(),
        }
    }

    pub fn with_options(options: CompilationOptions) -> Self {
        Self { options }
    }

    pub fn compile(&self, sources: &SourceMap) -> RatResult<Compilation> {
        let file = sources
            .iter()
            .next()
            .ok_or_else(|| RatError::error("no sources provided"))?;
        let tokens = lexer::lex(file)?;
        let program = parser::parse(&tokens)?;
        let type_info = typeck::check(&program)?;
        Ok(Compilation { program, type_info })
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
