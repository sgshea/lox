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
#[error("oops!")]
pub struct ParseError {
    #[source_code]
    src: NamedSource<String>,
    // Snippets and highlights can be included in the diagnostic!
    #[label("This bit here")]
    bad_bit: SourceSpan,
}

impl ParseError {
    pub fn new(src: NamedSource<String>, bad_bit: SourceSpan) -> Self {
        Self { src, bad_bit }
    }
}
