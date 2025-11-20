use miette::NamedSource;

use crate::{
    LoxError, LoxResult,
    chunk::{Chunk, Instruction, Value},
    error::ParseError,
    scanner::{Scanner, Token, TokenType},
};

#[derive(PartialEq, PartialOrd, Copy, Clone)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next(self) -> Self {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => Primary,
        }
    }
}

type ParseFn = for<'source> fn(&mut Parser<'source>, &mut Chunk, bool);

#[derive(Copy, Clone)]
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    fn get_rule(token_type: TokenType) -> Self {
        use TokenType::*;

        match token_type {
            LeftParen => Self {
                prefix: Some(grouping),
                infix: None,
                precedence: Precedence::Call,
            },
            Minus => Self {
                prefix: Some(unary),
                infix: Some(binary),
                precedence: Precedence::Term,
            },
            Plus => Self {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Term,
            },
            Slash => Self {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            },
            Star => Self {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            },
            Number => Self {
                prefix: Some(number),
                infix: None,
                precedence: Precedence::Primary,
            },
            _ => Self {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}

pub struct Parser<'source> {
    pub source_name: &'source str,
    pub scanner: Scanner<'source>,

    pub current: Token<'source>,
    pub previous: Token<'source>,

    pub errors: Vec<LoxError>,
}

impl<'source> Parser<'source> {
    pub fn new(source_code: &'source str, source_name: &'source str) -> Self {
        Self {
            source_name,
            scanner: Scanner::new(source_code),
            current: Self::sentinel_token(),
            previous: Self::sentinel_token(),
            errors: Vec::new(),
        }
    }

    fn sentinel_token() -> Token<'source> {
        Token {
            kind: TokenType::Error,
            lexeme: "",
            line: 0,
            span: (0, 0),
        }
    }

    /// Advances the parser to the next token, reporting any scanning errors along the way.
    pub fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = self.scanner.scan_token();
            if !self.current.is_token(TokenType::Error) {
                break;
            }

            self.error_at_current(self.current.lexeme);
        }
    }

    pub fn consume(&mut self, expected: TokenType, message: &str) {
        if self.current.is_token(expected) {
            self.advance();
        } else {
            self.error_at_current(message);
        }
    }

    fn expression(&mut self, chunk: &mut Chunk) {
        self.parse_precedence(chunk, Precedence::Assignment);
    }

    fn parse_precedence(&mut self, chunk: &mut Chunk, precedence: Precedence) {
        self.advance();
        let prefix_rule = ParseRule::get_rule(self.previous.kind).prefix;
        let can_assign = precedence <= Precedence::Assignment;

        if let Some(prefix_fn) = prefix_rule {
            prefix_fn(self, chunk, can_assign);
        } else {
            self.error_at_previous("Expect expression.");
            return;
        }

        while precedence <= ParseRule::get_rule(self.current.kind).precedence {
            self.advance();
            if let Some(infix_fn) = ParseRule::get_rule(self.previous.kind).infix {
                infix_fn(self, chunk, can_assign);
            } else {
                break;
            }
        }

        if can_assign && self.current.is_token(TokenType::Equal) {
            self.error_at_current("Invalid assignment target.");
            self.advance();
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message);
    }

    fn error_at_previous(&mut self, message: &str) {
        self.error_at(self.previous, message);
    }

    fn error_at(&mut self, token: Token<'source>, message: &str) {
        self.errors.push(LoxError::ParseError(ParseError::new(
            NamedSource::new(self.source_name, self.scanner.source.to_string()),
            token.span.into(),
            message.to_string(),
        )));
    }
}

/// Compiles the given source code into bytecode and writes it to the provided chunk.
/// Currently, only single expressions are supported (Chapter 17).
pub fn compile(source: &str, source_name: &str, chunk: &mut Chunk) -> LoxResult<()> {
    let mut parser = Parser::new(source, source_name);

    parser.advance();
    parser.expression(chunk);

    if parser.current.is_token(TokenType::Semicolon) {
        parser.advance();
    }

    parser.consume(TokenType::Eof, "Expect end of expression.");

    chunk.emit_instructions(parser.previous.line, &[Instruction::Return]);

    if parser.errors.is_empty() {
        Ok(())
    } else {
        Err(parser.errors)
    }
}

fn number(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    let literal = parser.previous.lexeme;
    match literal.parse::<f64>() {
        Ok(value) => {
            chunk.write_constant(Value::Number(value), parser.previous.line);
        }
        Err(_) => parser.error_at_previous("Invalid number literal."),
    }
}

fn grouping(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    parser.expression(chunk);
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

fn unary(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    let operator = parser.previous;
    parser.parse_precedence(chunk, Precedence::Unary);

    match operator.kind {
        TokenType::Minus => {
            chunk.write(Instruction::Negate, operator.line);
        }
        _ => parser.error_at_previous("Unsupported unary operator."),
    }
}

fn binary(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    let operator = parser.previous;
    let rule = ParseRule::get_rule(operator.kind);
    parser.parse_precedence(chunk, rule.precedence.next());

    match operator.kind {
        TokenType::Plus => {
            chunk.write(Instruction::Add, operator.line);
        }
        TokenType::Minus => {
            chunk.write(Instruction::Sub, operator.line);
        }
        TokenType::Star => {
            chunk.write(Instruction::Mul, operator.line);
        }
        TokenType::Slash => {
            chunk.write(Instruction::Div, operator.line);
        }
        _ => parser.error_at_previous("Unsupported binary operator."),
    }
}
