use miette::NamedSource;

use crate::{
    LoxError, LoxResult,
    chunk::{Chunk, Instruction},
    error::ParseError,
    scanner::{Scanner, Token, TokenType},
};

pub struct Parser<'source> {
    pub source_name: &'source str,
    pub scanner: Scanner<'source>,

    pub current: Token<'source>,
    pub previous: Token<'source>,

    pub errors: Vec<LoxError>,
}

impl<'source> Parser<'source> {
    /// Starts by parsing first tokens
    /// If either tokens are error tokens, returns an error
    /// @param sourcecode The source code to parse
    /// @param sourcename The name of the source file
    pub fn start(source_code: &'source str, source_name: &'source str) -> Self {
        let mut errors = Vec::new();
        let mut scanner = Scanner::new(source_code);
        let first_token = scanner.scan_token();
        if first_token.is_token(TokenType::Error) {
            errors.push(first_token.to_error(&source_name, source_code));
        }
        let second_token = scanner.scan_token();
        if second_token.is_token(TokenType::Error) {
            errors.push(second_token.to_error(&source_name, source_code));
        }
        Self {
            source_name,
            scanner,
            current: second_token,
            previous: first_token,
            errors,
        }
    }

    /// Advances the parser to the next token
    pub fn advance(&mut self) {
        self.previous = self.current;
        self.current = self.scanner.scan_token();
        if self.current.is_token(TokenType::Error) {
            self.errors.push(
                self.current
                    .to_error(&self.source_name, self.scanner.source),
            );
        }
    }

    /// Consumes next token, similar to advance but has an expected token
    /// @param expected The expected token type
    pub fn consume(&mut self, expected: TokenType) {
        if self.current.is_token(expected) {
            self.advance();
        } else {
            // Push error
            self.errors.push(LoxError::ParseError(ParseError::new(
                NamedSource::new(self.source_name, self.scanner.source.to_string()),
                self.current.span.into(),
            )));
        }
    }
}

/// Compiles the given source code into bytecode and writes it to the provided chunk.
/// @param source The source code to compile
/// @param sourcename The name of the source file
/// @param chunk The chunk to write the bytecode to
pub fn compile(source: &str, source_name: &str, chunk: &mut Chunk) -> LoxResult<()> {
    let mut parser = Parser::start(source, source_name);

    while !parser.current.is_token(TokenType::Error) {
        parser.advance();
    }

    // Add return byte
    chunk.emit_bytes(parser.current.line, &[Instruction::Return]);

    // If there are errors, return them
    if !parser.errors.is_empty() {
        Err(parser.errors)
    } else {
        Ok(())
    }
}
