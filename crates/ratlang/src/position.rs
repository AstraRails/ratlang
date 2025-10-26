use serde::{Deserialize, Serialize};
use std::fmt;

/// A zero-based offset into a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash, Default)]
pub struct Offset(pub usize);

impl Offset {
    #[inline]
    pub fn new(value: usize) -> Self {
        Self(value)
    }

    #[inline]
    pub fn value(self) -> usize {
        self.0
    }
}

/// Identifier for a source file within a [`SourceMap`](crate::source::SourceMap).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct FileId(pub u32);

impl FileId {
    #[inline]
    pub fn new(raw: u32) -> Self {
        Self(raw)
    }

    #[inline]
    pub fn raw(self) -> u32 {
        self.0
    }
}

/// A position expressed as 1-based line/column pairs (matching what users see
/// in editors and error messages).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash, Default)]
pub struct Position {
    pub line: u32,
    pub column: u32,
    pub offset: Offset,
}

impl Position {
    #[inline]
    pub fn new(line: u32, column: u32, offset: usize) -> Self {
        Self {
            line,
            column,
            offset: Offset(offset),
        }
    }

    #[inline]
    pub fn zero() -> Self {
        Self::new(1, 1, 0)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A contiguous region within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash, Default)]
pub struct Span {
    pub file_id: FileId,
    pub start: Position,
    pub end: Position,
}

impl Span {
    #[inline]
    pub fn new(file_id: FileId, start: Position, end: Position) -> Self {
        Self { file_id, start, end }
    }

    #[inline]
    pub fn single(file_id: FileId, pos: Position) -> Self {
        Self {
            file_id,
            start: pos,
            end: pos,
        }
    }

    #[inline]
    pub fn union(self, other: Span) -> Span {
        debug_assert_eq!(self.file_id, other.file_id);
        let start = if self.start.offset.value() <= other.start.offset.value() {
            self.start
        } else {
            other.start
        };
        let end = if self.end.offset.value() >= other.end.offset.value() {
            self.end
        } else {
            other.end
        };
        Span {
            file_id: self.file_id,
            start,
            end,
        }
    }

    #[inline]
    pub fn contains_offset(self, offset: usize) -> bool {
        offset >= self.start.offset.value() && offset <= self.end.offset.value()
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start == self.end {
            write!(f, "{}", self.start)
        } else {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}

/// A span that is absent / unknown (used for synthesized nodes).
pub const DUMMY_SPAN: Span = Span {
    file_id: FileId(0),
    start: Position {
        line: 0,
        column: 0,
        offset: Offset(0),
    },
    end: Position {
        line: 0,
        column: 0,
        offset: Offset(0),
    },
};
