use crate::diagnostics::{RatError, RatResult};
use crate::source::SourceMap;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct DocBundle {
    pub markdown: String,
}

pub fn generate(sources: &SourceMap) -> RatResult<DocBundle> {
    let mut markdown = String::new();
    writeln!(&mut markdown, "# Ratlang Documentation\n")
        .map_err(|err| RatError::error(err.to_string()))?;
    for file in sources.iter() {
        writeln!(&mut markdown, "## {}\n", &*file.name)
            .map_err(|err| RatError::error(err.to_string()))?;
        writeln!(&mut markdown, "```ratlang\n{}\n```\n", &*file.text)
            .map_err(|err| RatError::error(err.to_string()))?;
    }
    Ok(DocBundle { markdown })
}
