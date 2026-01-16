use miette::NamedSource;
use std::rc::Rc;

use crate::{
    LoxError, LoxResult,
    chunk::{Chunk, Instruction, Value},
    error::ParseError,
    object::{LoxFunction, StringInterner},
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

type ParseFn = for<'source> fn(&mut Parser<'source>, bool);

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
                infix: Some(call),
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

/// Type of function being compiled
#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
    Script,
    Function,
}

/// Compiler state for tracking local variables and scope
struct Compiler<'source> {
    /// The enclosing compiler (for nested functions)
    enclosing: Option<Box<Compiler<'source>>>,
    /// The function being compiled
    function: LoxFunction,
    /// The type of function being compiled
    function_type: FunctionType,
    /// Local variables in scope
    locals: Vec<Local<'source>>,
    /// Current scope depth
    scope_depth: i32,
}

impl<'source> Compiler<'source> {
    fn new(function_type: FunctionType) -> Self {
        let mut compiler = Self {
            enclosing: None,
            function: LoxFunction::new(),
            function_type,
            locals: Vec::new(),
            scope_depth: 0,
        };

        // Reserve slot 0
        compiler.locals.push(Local {
            name: Token {
                kind: TokenType::Error,
                lexeme: "",
                line: 0,
                span: (0, 0),
            },
            depth: 0,
        });

        compiler
    }

    fn new_with_name(function_type: FunctionType, name: Rc<String>) -> Self {
        let mut compiler = Self::new(function_type);
        compiler.function.name = Some(name);
        compiler
    }

    /// Get a mutable reference to the current chunk
    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
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
            compiler: Compiler::new(FunctionType::Script),
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

    /// Get a mutable reference to the current chunk
    fn current_chunk(&mut self) -> &mut Chunk {
        self.compiler.current_chunk()
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

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        // Synchronize on errors
        if !self.errors.is_empty() {
            self.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn function(&mut self, function_type: FunctionType) {
        let name = self.interner.intern(self.previous.lexeme);

        let new_compiler = Compiler::new_with_name(function_type, name);

        // Swap in the new compiler, storing the old one as enclosing
        let old_compiler = std::mem::replace(&mut self.compiler, new_compiler);
        self.compiler.enclosing = Some(Box::new(old_compiler));

        self.begin_scope();

        // Parse parameters
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                if self.compiler.function.arity == u8::MAX {
                    self.error_at_current("Can't have more than 255 parameters.");
                }
                self.compiler.function.arity += 1;
                let param = self.parse_variable("Expect parameter name.");
                self.define_variable(param);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");

        // Parse body
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        // End compiler and get the function
        let function = self.end_compiler();

        // Restore the enclosing compiler
        let enclosing = self.compiler.enclosing.take().expect("No enclosing compiler");
        self.compiler = *enclosing;

        // Emit the function as a constant
        let constant = self.current_chunk().add_constant(Value::Function(Rc::new(function)));
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Constant(constant), line);
    }

    fn end_compiler(&mut self) -> LoxFunction {
        self.emit_return();

        // Take the function out of the compiler
        std::mem::take(&mut self.compiler.function)
    }

    fn emit_return(&mut self) {
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Nil, line);
        self.current_chunk().write(Instruction::Return, line);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            let line = self.previous.line;
            self.current_chunk().write(Instruction::Nil, line);
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
        self.define_variable(global);
    }

    fn parse_variable(&mut self, error_message: &str) -> usize {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(self.previous)
    }

    fn identifier_constant(&mut self, name: Token<'source>) -> usize {
        let interned = self.interner.intern(name.lexeme);
        self.current_chunk().add_constant(Value::String(interned))
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

    fn define_variable(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        let line = self.previous.line;
        self.current_chunk().write(Instruction::DefineGlobal(global), line);
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        if let Some(local) = self.compiler.locals.last_mut() {
            local.depth = self.compiler.scope_depth;
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::Return) {
            self.return_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn return_statement(&mut self) {
        if self.compiler.function_type == FunctionType::Script {
            self.error_at_previous("Can't return from top-level code.");
        }

        if self.match_token(TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            let line = self.previous.line;
            self.current_chunk().write(Instruction::Return, line);
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Print, line);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Pop, line);
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        // Pop all locals from this scope
        while !self.compiler.locals.is_empty()
            && self.compiler.locals.last().unwrap().depth > self.compiler.scope_depth
        {
            let line = self.previous.line;
            self.current_chunk().write(Instruction::Pop, line);
            self.compiler.locals.pop();
        }
    }

    /// Emits a jump instruction with a placeholder offset, returns the index of the instruction
    fn emit_jump(&mut self, instruction: Instruction) -> usize {
        let line = self.previous.line;
        self.current_chunk().write(instruction, line)
    }

    /// Patches a previously emitted jump instruction with the correct offset
    fn patch_jump(&mut self, offset: usize) {
        // Calculate jump distance: from after the jump instruction to current position
        let jump = self.current_chunk().code.len() - offset - 1;

        if jump > u16::MAX as usize {
            self.error_at_previous("Too much code to jump over.");
            return;
        }

        // Update the instruction with the actual offset
        match self.current_chunk().code[offset] {
            Instruction::Jump(_) | Instruction::JumpIfFalse(_) => {
                match &mut self.current_chunk().code[offset] {
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
    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().code.len() - loop_start + 1;

        if offset > u16::MAX as usize {
            self.error_at_previous("Loop body too large.");
            return;
        }

        let line = self.previous.line;
        self.current_chunk().write(Instruction::Loop(offset as u16), line);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // Jump over the then branch if condition is falsey
        let then_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Pop, line); // Pop condition if true
        self.statement();

        // Jump over the else branch after executing then branch
        let else_jump = self.emit_jump(Instruction::Jump(0));

        self.patch_jump(then_jump);
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Pop, line); // Pop condition if false

        if self.match_token(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // Jump past the body if condition is falsey
        let exit_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Pop, line); // Pop condition
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Pop, line); // Pop condition on exit
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

        // Initializer clause
        if self.match_token(TokenType::Semicolon) {
            // No initializer
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().code.len();

        // Condition clause
        let mut exit_jump = None;
        if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if condition is false
            exit_jump = Some(self.emit_jump(Instruction::JumpIfFalse(0)));
            let line = self.previous.line;
            self.current_chunk().write(Instruction::Pop, line); // Pop condition
        }

        // Increment clause
        if !self.match_token(TokenType::RightParen) {
            // Jump over increment, run body, then come back
            let body_jump = self.emit_jump(Instruction::Jump(0));
            let increment_start = self.current_chunk().code.len();

            self.expression();
            let line = self.previous.line;
            self.current_chunk().write(Instruction::Pop, line); // Pop increment result
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        // Body
        self.statement();
        self.emit_loop(loop_start);

        // Patch exit jump if there was a condition
        if let Some(exit) = exit_jump {
            self.patch_jump(exit);
            let line = self.previous.line;
            self.current_chunk().write(Instruction::Pop, line); // Pop condition on exit
        }

        self.end_scope();
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

    fn named_variable(&mut self, name: Token<'source>, can_assign: bool) {
        let (get_op, set_op) = match self.resolve_local(&name) {
            Some(slot) => (Instruction::GetLocal(slot), Instruction::SetLocal(slot)),
            None => {
                let arg = self.identifier_constant(name);
                (Instruction::GetGlobal(arg), Instruction::SetGlobal(arg))
            }
        };

        let line = self.previous.line;
        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.current_chunk().write(set_op, line);
        } else {
            self.current_chunk().write(get_op, line);
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

    fn argument_list(&mut self) -> u8 {
        let mut arg_count: u8 = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error_at_previous("Can't have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = ParseRule::get_rule(self.previous.kind).prefix;
        let can_assign = precedence <= Precedence::Assignment;

        if let Some(prefix_fn) = prefix_rule {
            prefix_fn(self, can_assign);
        } else {
            self.error_at_previous("Expect expression.");
            return;
        }

        while precedence <= ParseRule::get_rule(self.current.kind).precedence {
            self.advance();
            if let Some(infix_fn) = ParseRule::get_rule(self.previous.kind).infix {
                infix_fn(self, can_assign);
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

/// Compiles the given source code into a function.
pub fn compile(source: &str, source_name: &str) -> LoxResult<Rc<LoxFunction>> {
    let mut parser = Parser::new(source, source_name);

    parser.advance();

    while !parser.current.is_token(TokenType::Eof) {
        parser.declaration();
    }

    parser.consume(TokenType::Eof, "Expect end of expression.");

    let function = parser.end_compiler();

    if parser.errors.is_empty() {
        Ok(Rc::new(function))
    } else {
        Err(parser.errors)
    }
}

fn number(parser: &mut Parser<'_>, _can_assign: bool) {
    let literal = parser.previous.lexeme;
    let line = parser.previous.line;
    match literal.parse::<f64>() {
        Ok(value) => {
            parser.current_chunk().write_constant(Value::Number(value), line);
        }
        Err(_) => parser.error_at_previous("Invalid number literal."),
    }
}

fn grouping(parser: &mut Parser<'_>, _can_assign: bool) {
    parser.expression();
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

fn call(parser: &mut Parser<'_>, _can_assign: bool) {
    let arg_count = parser.argument_list();
    let line = parser.previous.line;
    parser.current_chunk().write(Instruction::Call(arg_count), line);
}

fn unary(parser: &mut Parser<'_>, _can_assign: bool) {
    let operator = parser.previous;
    parser.parse_precedence(Precedence::Unary);

    let line = operator.line;
    match operator.kind {
        TokenType::Minus => {
            parser.current_chunk().write(Instruction::Negate, line);
        }
        TokenType::Bang => {
            parser.current_chunk().write(Instruction::Not, line);
        }
        _ => parser.error_at_previous("Unsupported unary operator."),
    }
}

fn binary(parser: &mut Parser<'_>, _can_assign: bool) {
    let operator = parser.previous;
    let rule = ParseRule::get_rule(operator.kind);
    parser.parse_precedence(rule.precedence.next());

    let line = operator.line;
    match operator.kind {
        TokenType::Plus => {
            parser.current_chunk().write(Instruction::Add, line);
        }
        TokenType::Minus => {
            parser.current_chunk().write(Instruction::Sub, line);
        }
        TokenType::Star => {
            parser.current_chunk().write(Instruction::Mul, line);
        }
        TokenType::Slash => {
            parser.current_chunk().write(Instruction::Div, line);
        }
        TokenType::EqualEqual => {
            parser.current_chunk().write(Instruction::Equal, line);
        }
        TokenType::BangEqual => {
            parser.current_chunk().write(Instruction::Equal, line);
            parser.current_chunk().write(Instruction::Not, line);
        }
        TokenType::Greater => {
            parser.current_chunk().write(Instruction::Greater, line);
        }
        TokenType::GreaterEqual => {
            parser.current_chunk().write(Instruction::Less, line);
            parser.current_chunk().write(Instruction::Not, line);
        }
        TokenType::Less => {
            parser.current_chunk().write(Instruction::Less, line);
        }
        TokenType::LessEqual => {
            parser.current_chunk().write(Instruction::Greater, line);
            parser.current_chunk().write(Instruction::Not, line);
        }
        _ => parser.error_at_previous("Unsupported binary operator."),
    }
}

fn string(parser: &mut Parser<'_>, _can_assign: bool) {
    let lexeme = parser.previous.lexeme;
    let line = parser.previous.line;
    // Remove surrounding quotes
    let string_content = &lexeme[1..lexeme.len() - 1];
    // Intern the string
    let interned = parser.interner.intern(string_content);
    // Write as constant
    parser.current_chunk().write_constant(Value::String(interned), line);
}

fn literal(parser: &mut Parser<'_>, _can_assign: bool) {
    let line = parser.previous.line;
    match parser.previous.kind {
        TokenType::False => {
            parser.current_chunk().write_constant(Value::Bool(false), line);
        }
        TokenType::True => {
            parser.current_chunk().write_constant(Value::Bool(true), line);
        }
        TokenType::Nil => {
            parser.current_chunk().write_constant(Value::Nil, line);
        }
        _ => parser.error_at_previous("Unsupported literal."),
    }
}

fn variable(parser: &mut Parser<'_>, can_assign: bool) {
    let name = parser.previous;
    parser.named_variable(name, can_assign);
}

fn and_(parser: &mut Parser<'_>, _can_assign: bool) {
    // Left operand is already on the stack
    // Short-circuit: if falsey, skip right operand
    let end_jump = parser.emit_jump(Instruction::JumpIfFalse(0));

    let line = parser.previous.line;
    parser.current_chunk().write(Instruction::Pop, line); // Pop left operand if truthy
    parser.parse_precedence(Precedence::And);

    parser.patch_jump(end_jump);
    // If we jumped, the falsey left operand is still on stack as the result
    // If we didn't jump, the right operand is now on stack as the result
}

fn or_(parser: &mut Parser<'_>, _can_assign: bool) {
    // Left operand already on the stack
    // Short-circuit: if falsey, evaluate right; if truthy, skip right
    let else_jump = parser.emit_jump(Instruction::JumpIfFalse(0));
    let end_jump = parser.emit_jump(Instruction::Jump(0));

    parser.patch_jump(else_jump);
    let line = parser.previous.line;
    parser.current_chunk().write(Instruction::Pop, line); // Pop left operand if falsey

    parser.parse_precedence(Precedence::Or);

    parser.patch_jump(end_jump);
    // If left truthy, we jumped to end with it still on stack
    // If left falsey, we popped it and right operand is now on stack
}
