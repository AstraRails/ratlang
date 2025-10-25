use crate::diagnostics::RatResult;
use crate::source::SourceMap;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct DocBundle {
    pub markdown: String,
}

pub fn generate(sources: &SourceMap) -> RatResult<DocBundle> {
    let mut markdown = String::new();
    writeln!(&mut markdown, "# Ratlang Documentation\n")?;
    for file in sources.iter() {
        writeln!(&mut markdown, "## {}\n", file.name)?;
        writeln!(&mut markdown, "```ratlang\n{}\n```\n", file.text)?;
    }
    Ok(DocBundle { markdown })
}
