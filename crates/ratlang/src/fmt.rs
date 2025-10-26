use crate::diagnostics::RatResult;

/// Format Ratlang source using a minimal whitespace-normalising strategy.
///
/// For the initial release the formatter performs a lightweight normalisation
/// that trims trailing whitespace, ensures the file ends with a newline, and
/// collapses multiple blank lines. A fully fledged AST-aware formatter can build
/// on top of this foundation in future iterations.
pub fn format_source(source: &str) -> RatResult<String> {
    let mut lines: Vec<&str> = source.lines().collect();
    while matches!(lines.last(), Some(line) if line.trim().is_empty()) {
        lines.pop();
    }
    let formatted = lines
        .into_iter()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n");
    Ok(format!("{}\n", formatted))
}
