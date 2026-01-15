use std::collections::HashMap;

/// Represents a token in the source code.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

/// Represents a token in the source code.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'source> {
    /// The type of the token.
    pub kind: TokenType,
    /// The lexeme of the token.
    pub lexeme: &'source str,
    /// The line number where the token starts.
    pub line: usize,
    /// Span of the token (start and end on line)
    pub span: (usize, usize),
}

impl<'source> Token<'source> {
    fn make_token(
        kind: TokenType,
        lexeme: &'source str,
        line: usize,
        start: usize,
        end: usize,
    ) -> Self {
        Token {
            kind,
            lexeme,
            line,
            span: (start, end),
        }
    }

    pub fn is_token(&self, kind: TokenType) -> bool {
        self.kind == kind
    }
}

/// Represents a scanner to turn source code into tokens.
pub struct Scanner<'source> {
    /// The source code being scanned.
    pub source: &'source str,
    /// The starting index of the current token.
    start: usize,
    /// The current index in the source code.
    current: usize,
    /// The current line number.
    line: usize,

    /// The keywords of the language
    keywords: HashMap<&'source str, TokenType>,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Self {
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
            keywords: HashMap::from([
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
            ]),
        }
    }

    /// Makes a token with the given kind, lexeme, and line number.
    fn make_token(&self, kind: TokenType) -> Token<'source> {
        let lexeme = &self.source[self.start..self.current];
        Token::make_token(kind, lexeme, self.line, self.start, self.current)
    }

    /// Skips whitespace characters in the source code.
    fn skip_whitespace(&mut self) {
        while self.current < self.source.len() {
            match self.peek() {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'\n' => {
                    self.line += 1;
                    self.advance();
                }
                b'/' => {
                    if self.peek_next() == b'/' {
                        while self.peek() != b'\n' && self.current < self.source.len() {
                            self.advance();
                        }
                    } else {
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    /// Advances the current position in the source code.
    fn advance(&mut self) -> u8 {
        let c = self.peek();
        self.current += 1;
        c
    }

    fn match_char(&mut self, expected: u8) -> bool {
        if self.current >= self.source.len() {
            return false;
        }

        if self.source.as_bytes()[self.current] != expected {
            return false;
        }

        self.current += 1;
        true
    }

    /// Peeks at the next character in the source code.
    fn peek(&self) -> u8 {
        if self.current >= self.source.len() {
            return 0;
        }
        self.source.as_bytes()[self.current]
    }

    /// Peeks at the character after the next character in the source code.
    fn peek_next(&self) -> u8 {
        if self.current + 1 >= self.source.len() {
            return 0;
        }
        self.source.as_bytes()[self.current + 1]
    }

    fn string(&mut self) {
        while self.peek() != b'"' && self.peek() != 0 {
            self.advance();
        }
        if self.peek() == b'"' {
            self.advance();
        }
    }

    fn error(&mut self, message: &'source str) -> Token<'source> {
        Token {
            kind: TokenType::Error,
            lexeme: message,
            line: self.line,
            span: (self.start, self.current),
        }
    }

    fn number(&mut self) -> Token<'source> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token<'source> {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }
        // Try to make keyword
        if let Some(kw) = self.keywords.get(&self.source[self.start..self.current]) {
            self.make_token(*kw)
        } else {
            self.make_token(TokenType::Identifier)
        }
    }

    /// Scan the next token in sequence
    pub fn scan_token(&mut self) -> Token<'source> {
        self.skip_whitespace();
        self.start = self.current;
        if self.current >= self.source.len() {
            return self.make_token(TokenType::Eof);
        }

        match self.advance() {
            b'(' => self.make_token(TokenType::LeftParen),
            b')' => self.make_token(TokenType::RightParen),
            b'{' => self.make_token(TokenType::LeftBrace),
            b'}' => self.make_token(TokenType::RightBrace),
            b';' => self.make_token(TokenType::Semicolon),
            b',' => self.make_token(TokenType::Comma),
            b'.' => self.make_token(TokenType::Dot),
            b'-' => self.make_token(TokenType::Minus),
            b'+' => self.make_token(TokenType::Plus),
            b'/' => self.make_token(TokenType::Slash),
            b'*' => self.make_token(TokenType::Star),
            b'!' => {
                if self.match_char(b'=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            b'=' => {
                if self.match_char(b'=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            b'<' => {
                if self.match_char(b'=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            b'>' => {
                if self.match_char(b'=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            b'"' => {
                self.string();
                self.make_token(TokenType::String)
            }
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() || c == b'_' => self.identifier(), // Also handles keywords
            _ => self.error("Unexpected character."),
        }
    }
}
