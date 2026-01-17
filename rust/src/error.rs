use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum LoxError {
    #[error("Token error: {0}")]
    TokenError(String),

    #[error(transparent)]
    #[diagnostic(transparent)]
    RuntimeError(#[from] RuntimeError),

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

#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
#[diagnostic(help("{help}"))]
pub struct RuntimeError {
    #[source_code]
    src: Option<NamedSource<String>>,
    #[label("Here")]
    bad_bit: SourceSpan,
    message: String,
    help: String,
    line: usize,
}

impl RuntimeError {
    /// Create a new runtime error without source attached (for deferred attachment)
    pub fn new(message: impl Into<String>, span: (usize, usize), line: usize) -> Self {
        let (start, end) = span;
        let length = end.saturating_sub(start).max(1);
        Self {
            src: None,
            bad_bit: (start, length).into(),
            message: message.into(),
            help: String::new(),
            line,
        }
    }

    /// Create a runtime error with source already attached
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
            help: String::new(),
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

    /// Add a help message to this error
    pub fn with_help(self, help: impl Into<String>) -> Self {
        Self {
            help: help.into(),
            ..self
        }
    }

    /// Add a stack trace as help text
    pub fn with_stack_trace(self, stack_trace: impl Into<String>) -> Self {
        Self {
            help: stack_trace.into(),
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
