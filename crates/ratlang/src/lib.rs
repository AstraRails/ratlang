//! ratlang core library.
//!
//! This crate implements the Ratlang programming language toolchain including
//! the frontend (lexer, parser, type checker), intermediate representations,
//! executors (tree-walk interpreter, bytecode virtual machine, and native code
//! backends), and shared utilities used by the end-user tooling such as
//! `ratc`, `rat`, `ratpkg`, `ratfmt`, `ratdoc`, and the language server.
//!
//! The implementation focuses on delivering a pragmatic, production-ready
//! developer experience with strong safety guarantees, expressive concurrency,
//! and zero-cost abstractions. Even though Ratlang is a new language, the code
//! aims to be approachable to engineers already familiar with Rust, Python, or
//! Java, and borrows their best ideas while removing friction.

pub mod diagnostics;
pub mod position;
pub mod source;
pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod syntax;
pub mod types;
pub mod typeck;
pub mod hir;
pub mod ir;
pub mod bytecode;
pub mod runtime;
pub mod stdlib;
pub mod loader;
pub mod compiler;
pub mod fmt;
pub mod docs;
pub mod package;
pub mod language_server;
pub mod repl;

pub use compiler::{CompilationOptions, Compiler};
pub use diagnostics::{RatDiagnostic, RatError};
pub use runtime::{ExecutionConfig, Executor, ExecutorHandle, Value};
pub use source::{SourceFile, SourceId, SourceMap};
pub use tokens::{Keyword, Symbol, Token, TokenKind};

/// Version of the Ratlang toolchain crate.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Build metadata such as git commit hash if available.
pub const BUILD_META: &str = env!("CARGO_PKG_DESCRIPTION");

/// Entry point that lexes, parses, type-checks, and executes the provided
/// Ratlang source string using the default execution configuration. This is
/// primarily intended for tooling and quick evaluation / REPL flows.
pub async fn eval(source: &str) -> Result<Value, RatError> {
    let map = SourceMap::single("<stdin>", source);
    let compiler = Compiler::default();
    let program = compiler.frontend().compile(&map).await?;
    let mut executor = runtime::executor::ReplExecutor::new();
    executor.execute(&program, ExecutionConfig::default()).await
}

/// Convenience helper for formatting Ratlang source code using the canonical
/// style defined by `ratfmt`.
pub fn format(source: &str) -> Result<String, RatError> {
    fmt::format_source(source)
}

/// Generate documentation artifacts for the given Ratlang source map.
pub fn document(map: &SourceMap) -> Result<docs::DocBundle, RatError> {
    docs::generate(map)
}

/// Build bundle for packaging and distribution via `ratpkg`.
pub fn package(source_root: &std::path::Path) -> Result<package::PackageMetadata, RatError> {
    package::build_package(source_root)
}
