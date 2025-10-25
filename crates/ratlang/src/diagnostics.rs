use crate::position::{FileId, Position, Span};
use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Error,
    Warning,
    Note,
    Help,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Note => write!(f, "note"),
            Severity::Help => write!(f, "help"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Label {
    pub span: Span,
    pub message: Option<String>,
}

impl Label {
    pub fn new(span: Span, message: impl Into<Option<String>>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RatDiagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub primary: Option<Label>,
    pub secondary: Vec<Label>,
    pub notes: Vec<String>,
}

impl RatDiagnostic {
    pub fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self {
            severity,
            code: None,
            message: message.into(),
            primary: None,
            secondary: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self::new(Severity::Error, message)
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, message)
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_primary(mut self, span: Span, message: impl Into<Option<String>>) -> Self {
        self.primary = Some(Label::new(span, message));
        self
    }

    pub fn add_secondary(mut self, span: Span, message: impl Into<Option<String>>) -> Self {
        self.secondary.push(Label::new(span, message));
        self
    }

    pub fn add_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

impl fmt::Display for RatDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(code) = &self.code {
            write!(f, "[{code}] ")?;
        }
        write!(f, "{}: {}", self.severity, self.message)?;
        if let Some(primary) = &self.primary {
            write!(
                f,
                "\n --> {} at {}",
                primary.span.file_id.raw(),
                primary.span
            )?;
            if let Some(msg) = &primary.message {
                write!(f, "\n  | {msg}")?;
            }
        }
        for label in &self.secondary {
            write!(
                f,
                "\n  = note: {} at {}",
                label
                    .message
                    .as_deref()
                    .unwrap_or("related location"),
                label.span
            )?;
        }
        for note in &self.notes {
            write!(f, "\n  = note: {note}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum RatError {
    #[error("{0}")]
    Message(String),
    #[error("{0}")]
    Diagnostic(#[from] RatDiagnostic),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl RatError {
    pub fn diagnostic(severity: Severity, message: impl Into<String>) -> Self {
        RatDiagnostic::new(severity, message).into()
    }

    pub fn error(message: impl Into<String>) -> Self {
        RatDiagnostic::error(message).into()
    }
}

pub type RatResult<T, E = RatError> = std::result::Result<T, E>;

/// Convenience helper for creating an IO error diagnostic.
pub fn io_error(path: &std::path::Path, error: &std::io::Error) -> RatDiagnostic {
    RatDiagnostic::error(format!("I/O error accessing {}: {error}", path.display()))
        .with_code("EIO")
}

/// Helper to generate a simple span-less diagnostic.
pub fn simple_error(message: impl Into<String>) -> RatError {
    RatError::Diagnostic(RatDiagnostic::error(message))
}
