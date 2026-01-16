use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum LoxError {
    #[error("Token error: {0}")]
    TokenError(String),

    #[error("Runtime error")]
    RuntimeError(String),

    #[error(transparent)]
    #[diagnostic(transparent)]
    ParseError(#[from] ParseError),
}

#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
pub struct ParseError {
    #[source_code]
    src: Option<NamedSource<String>>,
    #[label("Here")]
    bad_bit: SourceSpan,
    message: String,
    line: usize,
}

impl ParseError {
    /// Create a new parse error without source attached (for deferred attachment)
    pub fn new(message: impl Into<String>, span: (usize, usize), line: usize) -> Self {
        let (start, end) = span;
        let length = end.saturating_sub(start).max(1);
        Self {
            src: None,
            bad_bit: (start, length).into(),
            message: message.into(),
            line,
        }
    }

    /// Create a parse error with source already attached
    pub fn with_source_immediate(
        src: NamedSource<String>,
        bad_bit: SourceSpan,
        message: impl Into<String>,
        line: usize,
    ) -> Self {
        Self {
            src: Some(src),
            bad_bit,
            message: message.into(),
            line,
        }
    }

    /// Attach source to this error (consumes and returns new error)
    pub fn with_source(self, src: NamedSource<String>) -> Self {
        Self {
            src: Some(src),
            ..self
        }
    }

    /// Get the line number where the error occurred
    pub fn line(&self) -> usize {
        self.line
    }

    /// Get the error message
    pub fn message(&self) -> &str {
        &self.message
    }
}
