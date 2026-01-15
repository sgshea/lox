use miette::NamedSource;

use crate::{
    LoxError, LoxResult,
    chunk::{Chunk, Instruction, Value},
    error::ParseError,
    object::StringInterner,
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
            And => Self {
                prefix: None,
                infix: Some(and_),
                precedence: Precedence::And,
            },
            Or => Self {
                prefix: None,
                infix: Some(or_),
                precedence: Precedence::Or,
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
            Slash | Star => Self {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            },
            Number => Self {
                prefix: Some(number),
                infix: None,
                precedence: Precedence::Primary,
            },
            String => Self {
                prefix: Some(string),
                infix: None,
                precedence: Precedence::Primary,
            },
            False | True | Nil => Self {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::Primary,
            },
            Bang => Self {
                prefix: Some(unary),
                infix: None,
                precedence: Precedence::None,
            },
            BangEqual | EqualEqual => Self {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Equality,
            },
            Greater | GreaterEqual | Less | LessEqual => Self {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            },
            Identifier => Self {
                prefix: Some(variable),
                infix: None,
                precedence: Precedence::None,
            },
            _ => Self {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}

/// Represents a local variable in the compiler
#[derive(Clone)]
struct Local<'source> {
    name: Token<'source>,
    depth: i32,
}

/// Compiler state for tracking local variables and scope
struct Compiler<'source> {
    locals: Vec<Local<'source>>,
    scope_depth: i32,
}

impl<'source> Compiler<'source> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            scope_depth: 0,
        }
    }
}

pub struct Parser<'source> {
    pub source_name: &'source str,
    pub scanner: Scanner<'source>,

    pub current: Token<'source>,
    pub previous: Token<'source>,

    pub errors: Vec<LoxError>,
    pub interner: StringInterner,

    compiler: Compiler<'source>,
}

impl<'source> Parser<'source> {
    pub fn new(source_code: &'source str, source_name: &'source str) -> Self {
        Self {
            source_name,
            scanner: Scanner::new(source_code),
            current: Self::sentinel_token(),
            previous: Self::sentinel_token(),
            errors: Vec::new(),
            interner: StringInterner::new(),
            compiler: Compiler::new(),
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

    fn check(&self, kind: TokenType) -> bool {
        self.current.is_token(kind)
    }

    fn match_token(&mut self, kind: TokenType) -> bool {
        if !self.check(kind) {
            return false;
        }
        self.advance();
        true
    }

    fn expression(&mut self, chunk: &mut Chunk) {
        self.parse_precedence(chunk, Precedence::Assignment);
    }

    fn declaration(&mut self, chunk: &mut Chunk) {
        if self.match_token(TokenType::Var) {
            self.var_declaration(chunk);
        } else {
            self.statement(chunk);
        }

        // Synchronize on errors
        if !self.errors.is_empty() {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self, chunk: &mut Chunk) {
        let global = self.parse_variable(chunk, "Expect variable name.");

        if self.match_token(TokenType::Equal) {
            self.expression(chunk);
        } else {
            chunk.write(Instruction::Nil, self.previous.line);
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
        self.define_variable(chunk, global);
    }

    fn parse_variable(&mut self, chunk: &mut Chunk, error_message: &str) -> usize {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(chunk, self.previous)
    }

    fn identifier_constant(&mut self, chunk: &mut Chunk, name: Token<'source>) -> usize {
        let interned = self.interner.intern(name.lexeme);
        chunk.add_constant(Value::String(interned))
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous;

        // Check for duplicate local variable in same scope
        let mut has_duplicate = false;
        for local in self.compiler.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }

            if Self::identifiers_equal(&name, &local.name) {
                has_duplicate = true;
                break;
            }
        }

        if has_duplicate {
            self.error_at_previous("Already a variable with this name in this scope.");
        }

        self.add_local(name);
    }

    fn identifiers_equal(a: &Token, b: &Token) -> bool {
        a.lexeme == b.lexeme
    }

    fn add_local(&mut self, name: Token<'source>) {
        let local = Local { name, depth: -1 }; // -1 marks uninitialized
        self.compiler.locals.push(local);
    }

    fn define_variable(&mut self, chunk: &mut Chunk, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        chunk.write(Instruction::DefineGlobal(global), self.previous.line);
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        if let Some(local) = self.compiler.locals.last_mut() {
            local.depth = self.compiler.scope_depth;
        }
    }

    fn statement(&mut self, chunk: &mut Chunk) {
        if self.match_token(TokenType::Print) {
            self.print_statement(chunk);
        } else if self.match_token(TokenType::If) {
            self.if_statement(chunk);
        } else if self.match_token(TokenType::While) {
            self.while_statement(chunk);
        } else if self.match_token(TokenType::For) {
            self.for_statement(chunk);
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block(chunk);
            self.end_scope(chunk);
        } else {
            self.expression_statement(chunk);
        }
    }

    fn print_statement(&mut self, chunk: &mut Chunk) {
        self.expression(chunk);
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        chunk.write(Instruction::Print, self.previous.line);
    }

    fn expression_statement(&mut self, chunk: &mut Chunk) {
        self.expression(chunk);
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        chunk.write(Instruction::Pop, self.previous.line);
    }

    fn block(&mut self, chunk: &mut Chunk) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration(chunk);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self, chunk: &mut Chunk) {
        self.compiler.scope_depth -= 1;

        // Pop all locals from this scope
        while !self.compiler.locals.is_empty()
            && self.compiler.locals.last().unwrap().depth > self.compiler.scope_depth
        {
            chunk.write(Instruction::Pop, self.previous.line);
            self.compiler.locals.pop();
        }
    }

    /// Emits a jump instruction with a placeholder offset, returns the index of the instruction
    fn emit_jump(&mut self, chunk: &mut Chunk, instruction: Instruction) -> usize {
        chunk.write(instruction, self.previous.line)
    }

    /// Patches a previously emitted jump instruction with the correct offset
    fn patch_jump(&mut self, chunk: &mut Chunk, offset: usize) {
        // Calculate jump distance: from after the jump instruction to current position
        let jump = chunk.code.len() - offset - 1;

        if jump > u16::MAX as usize {
            self.error_at_previous("Too much code to jump over.");
            return;
        }

        // Update the instruction with the actual offset
        match chunk.code[offset] {
            Instruction::Jump(_) | Instruction::JumpIfFalse(_) => {
                match &mut chunk.code[offset] {
                    Instruction::Jump(o) | Instruction::JumpIfFalse(o) => {
                        *o = jump as u16
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    /// Emits a loop instruction that jumps backward to loop_start
    fn emit_loop(&mut self, chunk: &mut Chunk, loop_start: usize) {
        let offset = chunk.code.len() - loop_start + 1;

        if offset > u16::MAX as usize {
            self.error_at_previous("Loop body too large.");
            return;
        }

        chunk.write(Instruction::Loop(offset as u16), self.previous.line);
    }

    fn if_statement(&mut self, chunk: &mut Chunk) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression(chunk);
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // Jump over the then branch if condition is falsey
        let then_jump = self.emit_jump(chunk, Instruction::JumpIfFalse(0));
        chunk.write(Instruction::Pop, self.previous.line); // Pop condition if true
        self.statement(chunk);

        // Jump over the else branch after executing then branch
        let else_jump = self.emit_jump(chunk, Instruction::Jump(0));

        self.patch_jump(chunk, then_jump);
        chunk.write(Instruction::Pop, self.previous.line); // Pop condition if false

        if self.match_token(TokenType::Else) {
            self.statement(chunk);
        }
        self.patch_jump(chunk, else_jump);
    }

    fn while_statement(&mut self, chunk: &mut Chunk) {
        let loop_start = chunk.code.len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression(chunk);
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // Jump past the body if condition is falsey
        let exit_jump = self.emit_jump(chunk, Instruction::JumpIfFalse(0));
        chunk.write(Instruction::Pop, self.previous.line); // Pop condition
        self.statement(chunk);
        self.emit_loop(chunk, loop_start);

        self.patch_jump(chunk, exit_jump);
        chunk.write(Instruction::Pop, self.previous.line); // Pop condition on exit
    }

    fn for_statement(&mut self, chunk: &mut Chunk) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

        // Initializer clause
        if self.match_token(TokenType::Semicolon) {
            // No initializer
        } else if self.match_token(TokenType::Var) {
            self.var_declaration(chunk);
        } else {
            self.expression_statement(chunk);
        }

        let mut loop_start = chunk.code.len();

        // Condition clause
        let mut exit_jump = None;
        if !self.match_token(TokenType::Semicolon) {
            self.expression(chunk);
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if condition is false
            exit_jump = Some(self.emit_jump(chunk, Instruction::JumpIfFalse(0)));
            chunk.write(Instruction::Pop, self.previous.line); // Pop condition
        }

        // Increment clause
        if !self.match_token(TokenType::RightParen) {
            // Jump over increment, run body, then come back
            let body_jump = self.emit_jump(chunk, Instruction::Jump(0));
            let increment_start = chunk.code.len();

            self.expression(chunk);
            chunk.write(Instruction::Pop, self.previous.line); // Pop increment result
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(chunk, loop_start);
            loop_start = increment_start;
            self.patch_jump(chunk, body_jump);
        }

        // Body
        self.statement(chunk);
        self.emit_loop(chunk, loop_start);

        // Patch exit jump if there was a condition
        if let Some(exit) = exit_jump {
            self.patch_jump(chunk, exit);
            chunk.write(Instruction::Pop, self.previous.line); // Pop condition on exit
        }

        self.end_scope(chunk);
    }

    fn synchronize(&mut self) {
        while !self.current.is_token(TokenType::Eof) {
            if self.previous.is_token(TokenType::Semicolon) {
                return;
            }

            match self.current.kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn named_variable(&mut self, chunk: &mut Chunk, name: Token<'source>, can_assign: bool) {
        let (get_op, set_op) = match self.resolve_local(&name) {
            Some(slot) => (Instruction::GetLocal(slot), Instruction::SetLocal(slot)),
            None => {
                let arg = self.identifier_constant(chunk, name);
                (Instruction::GetGlobal(arg), Instruction::SetGlobal(arg))
            }
        };

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression(chunk);
            chunk.write(set_op, self.previous.line);
        } else {
            chunk.write(get_op, self.previous.line);
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Option<usize> {
        for (i, local) in self.compiler.locals.iter().enumerate().rev() {
            if Self::identifiers_equal(name, &local.name) {
                if local.depth == -1 {
                    self.error_at_previous("Can't read local variable in its own initializer.");
                }
                return Some(i);
            }
        }
        None
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
        let (start, end) = token.span;
        let length = end.saturating_sub(start).max(1);
        self.errors.push(LoxError::ParseError(ParseError::new(
            NamedSource::new(self.source_name, self.scanner.source.to_string()),
            (start, length).into(),
            message.to_string(),
        )));
    }
}

/// Compiles the given source code into bytecode and writes it to the provided chunk.
pub fn compile(source: &str, source_name: &str, chunk: &mut Chunk) -> LoxResult<()> {
    let mut parser = Parser::new(source, source_name);

    parser.advance();

    while !parser.current.is_token(TokenType::Eof) {
        parser.declaration(chunk);
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
        TokenType::Bang => {
            chunk.write(Instruction::Not, operator.line);
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
        TokenType::EqualEqual => {
            chunk.write(Instruction::Equal, operator.line);
        }
        TokenType::BangEqual => {
            chunk.write(Instruction::Equal, operator.line);
            chunk.write(Instruction::Not, operator.line);
        }
        TokenType::Greater => {
            chunk.write(Instruction::Greater, operator.line);
        }
        TokenType::GreaterEqual => {
            chunk.write(Instruction::Less, operator.line);
            chunk.write(Instruction::Not, operator.line);
        }
        TokenType::Less => {
            chunk.write(Instruction::Less, operator.line);
        }
        TokenType::LessEqual => {
            chunk.write(Instruction::Greater, operator.line);
            chunk.write(Instruction::Not, operator.line);
        }
        _ => parser.error_at_previous("Unsupported binary operator."),
    }
}

fn string(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    let lexeme = parser.previous.lexeme;
    // Remove surrounding quotes
    let string_content = &lexeme[1..lexeme.len() - 1];
    // Intern the string
    let interned = parser.interner.intern(string_content);
    // Write as constant
    chunk.write_constant(Value::String(interned), parser.previous.line);
}

fn literal(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    match parser.previous.kind {
        TokenType::False => {
            chunk.write_constant(Value::Bool(false), parser.previous.line);
        }
        TokenType::True => {
            chunk.write_constant(Value::Bool(true), parser.previous.line);
        }
        TokenType::Nil => {
            chunk.write_constant(Value::Nil, parser.previous.line);
        }
        _ => parser.error_at_previous("Unsupported literal."),
    }
}

fn variable(parser: &mut Parser<'_>, chunk: &mut Chunk, can_assign: bool) {
    let name = parser.previous;
    parser.named_variable(chunk, name, can_assign);
}

fn and_(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    // Left operand is already on the stack
    // Short-circuit: if falsey, skip right operand
    let end_jump = parser.emit_jump(chunk, Instruction::JumpIfFalse(0));

    chunk.write(Instruction::Pop, parser.previous.line); // Pop left operand if truthy
    parser.parse_precedence(chunk, Precedence::And);

    parser.patch_jump(chunk, end_jump);
    // If we jumped, the falsey left operand is still on stack as the result
    // If we didn't jump, the right operand is now on stack as the result
}

fn or_(parser: &mut Parser<'_>, chunk: &mut Chunk, _can_assign: bool) {
    // Left operand already on the stack
    // Short-circuit: if falsey, evaluate right; if truthy, skip right
    let else_jump = parser.emit_jump(chunk, Instruction::JumpIfFalse(0));
    let end_jump = parser.emit_jump(chunk, Instruction::Jump(0));

    parser.patch_jump(chunk, else_jump);
    chunk.write(Instruction::Pop, parser.previous.line); // Pop left operand if falsey

    parser.parse_precedence(chunk, Precedence::Or);

    parser.patch_jump(chunk, end_jump);
    // If left truthy, we jumped to end with it still on stack
    // If left falsey, we popped it and right operand is now on stack
}
