use thiserror::Error;

/// Error type for the Lox interpreter
#[derive(Error, Debug)]
pub enum LoxError {
    #[error("Token error: {0}")]
    TokenError(String),

    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Runtime error: {0}")]
    RuntimeError(String),
}
