use crate::position::{FileId, Position, Span};
use std::collections::HashMap;
use std::sync::Arc;

pub type SourceId = FileId;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: SourceId,
    pub name: Arc<str>,
    pub text: Arc<str>,
    line_starts: Arc<Vec<usize>>,
}

impl SourceFile {
    pub fn new(id: SourceId, name: impl Into<Arc<str>>, text: impl Into<Arc<str>>) -> Self {
        let text = text.into();
        let mut line_starts = Vec::with_capacity(text.len() / 32 + 1);
        line_starts.push(0);
        for (idx, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(idx + 1);
            }
        }
        Self {
            id,
            name: name.into(),
            text,
            line_starts: Arc::new(line_starts),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.text.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    #[inline]
    pub fn line_starts(&self) -> &[usize] {
        &self.line_starts
    }

    pub fn position(&self, offset: usize) -> Position {
        let mut low = 0usize;
        let mut high = self.line_starts.len();
        while low + 1 < high {
            let mid = (low + high) / 2;
            if self.line_starts[mid] <= offset {
                low = mid;
            } else {
                high = mid;
            }
        }
        let line_index = low as u32;
        let line_offset = self.line_starts[line_index as usize];
        let line = line_index + 1;
        let column = (offset - line_offset) as u32 + 1;
        Position::new(line, column, offset)
    }

    pub fn span(&self, start: usize, end: usize) -> Span {
        let start_pos = self.position(start);
        let end_pos = self.position(end);
        Span::new(self.id, start_pos, end_pos)
    }
}

#[derive(Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
    files_by_name: HashMap<Arc<str>, SourceId>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn single(name: impl Into<Arc<str>>, text: impl Into<Arc<str>>) -> Self {
        let mut map = SourceMap::new();
        let id = map.add(name, text);
        debug_assert_eq!(id.raw(), 0);
        map
    }

    pub fn add(&mut self, name: impl Into<Arc<str>>, text: impl Into<Arc<str>>) -> SourceId {
        let name_arc = name.into();
        if let Some(id) = self.files_by_name.get(&name_arc) {
            return *id;
        }
        let id = SourceId::new(self.files.len() as u32);
        let file = SourceFile::new(id, name_arc.clone(), text);
        self.files_by_name.insert(name_arc, id);
        self.files.push(file);
        id
    }

    pub fn get(&self, id: SourceId) -> Option<&SourceFile> {
        self.files.get(id.raw() as usize)
    }

    pub fn get_by_name(&self, name: &str) -> Option<&SourceFile> {
        self.files_by_name
            .get(name)
            .and_then(|id| self.get(*id))
    }

    pub fn iter(&self) -> impl Iterator<Item = &SourceFile> {
        self.files.iter()
    }

    pub fn span(&self, id: SourceId, start: usize, end: usize) -> Option<Span> {
        self.get(id).map(|file| file.span(start, end))
    }

    pub fn lookup_position(&self, span: Span) -> Option<(&SourceFile, Position, Position)> {
        self.get(span.file_id).map(|file| (file, span.start, span.end))
    }
}
