use miette::NamedSource;
use std::rc::Rc;

use crate::{
    LoxError, LoxResult,
    chunk::{Chunk, Instruction, Value},
    error::ParseError,
    object::{CompilerFunction, StringInterner, UpvalueDescriptor},
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
            This => Self {
                prefix: Some(this_),
                infix: None,
                precedence: Precedence::None,
            },
            Super => Self {
                prefix: Some(super_),
                infix: None,
                precedence: Precedence::None,
            },
            Dot => Self {
                prefix: None,
                infix: Some(dot),
                precedence: Precedence::Call,
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
    /// Whether this local is captured by a closure
    is_captured: bool,
}

/// Compiler-time representation of an upvalue
#[derive(Clone, Copy)]
struct Upvalue {
    /// Index of the local or upvalue being captured
    index: u8,
    /// True if capturing a local, false if capturing an upvalue
    is_local: bool,
}

/// Type of function being compiled
#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
    Script,
    Function,
    Method,
    Initializer,
}

/// Compiler state for tracking local variables and scope
struct Compiler<'source> {
    /// The enclosing compiler (for nested functions)
    enclosing: Option<Box<Compiler<'source>>>,
    /// The function being compiled
    function: CompilerFunction,
    /// The type of function being compiled
    function_type: FunctionType,
    /// Local variables in scope
    locals: Vec<Local<'source>>,
    /// Upvalues captured by this function
    upvalues: Vec<Upvalue>,
    /// Current scope depth
    scope_depth: i32,
}

impl<'source> Compiler<'source> {
    fn new(function_type: FunctionType) -> Self {
        let mut compiler = Self {
            enclosing: None,
            function: CompilerFunction::new(),
            function_type,
            locals: Vec::new(),
            upvalues: Vec::new(),
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
            is_captured: false,
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

/// Tracks class compilation context
struct ClassCompiler {
    enclosing: Option<Box<ClassCompiler>>,
    has_superclass: bool,
}

pub struct Parser<'source> {
    pub scanner: Scanner<'source>,

    pub current: Token<'source>,
    pub previous: Token<'source>,

    pub errors: Vec<LoxError>,
    pub interner: StringInterner,

    compiler: Compiler<'source>,
    current_class: Option<ClassCompiler>,
}

impl<'source> Parser<'source> {
    pub fn new(source_code: &'source str, _source_name: &'source str) -> Self {
        Self {
            scanner: Scanner::new(source_code),
            current: Self::sentinel_token(),
            previous: Self::sentinel_token(),
            errors: Vec::new(),
            interner: StringInterner::new(),
            compiler: Compiler::new(FunctionType::Script),
            current_class: None,
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

    /// Emit an instruction with span information from the previous token
    fn emit(&mut self, instruction: Instruction) {
        let line = self.previous.line;
        let span = self.previous.span;
        self.current_chunk().write_with_span(instruction, line, span);
    }

    /// Emit an instruction with span information from a specific token
    fn emit_at(&mut self, instruction: Instruction, token: Token) {
        self.current_chunk().write_with_span(instruction, token.line, token.span);
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
        if self.match_token(TokenType::Class) {
            self.class_declaration();
        } else if self.match_token(TokenType::Fun) {
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

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        let class_name = self.previous;
        let name_constant = self.identifier_constant(class_name);
        self.declare_variable();

        // Emit OP_CLASS instruction
        self.emit_at(Instruction::Class(name_constant), class_name);
        self.define_variable(name_constant);

        // Push class compiler context
        let previous_class = self.current_class.take();
        self.current_class = Some(ClassCompiler {
            enclosing: previous_class.map(Box::new),
            has_superclass: false,
        });

        // Check for superclass clause
        if self.match_token(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            
            // Load superclass by name (handles local/global)
            variable(self, false);
            
            // Prevent self-inheritance
            if Self::identifiers_equal(&class_name, &self.previous) {
                self.error_at_previous("A class can't inherit from itself.");
            }
            
            // Create scope for "super" variable
            self.begin_scope();
            self.add_local(self.synthetic_token("super"));
            self.define_variable(0);
            
            // Load subclass and emit inherit instruction
            self.named_variable(class_name, false);
            self.emit(Instruction::Inherit);
            
            if let Some(ref mut class) = self.current_class {
                class.has_superclass = true;
            }
        }

        // Parse class body
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");
        
        // Load the class back onto stack for method definitions
        // Use named_variable to support local classes
        self.named_variable(class_name, false);

        // Parse methods
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.consume(TokenType::Identifier, "Expect method name.");
            let method_name = self.previous;
            let method_name_constant = self.identifier_constant(method_name);

            self.method(method_name_constant);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.");

        // Pop the class from stack
        self.emit(Instruction::Pop);

        // Close superclass scope if needed
        if self.current_class.as_ref().map_or(false, |c| c.has_superclass) {
            self.end_scope();
        }

        // Pop class compiler context
        if let Some(class) = self.current_class.take() {
            self.current_class = class.enclosing.map(|b| *b);
        }
    }

    fn method(&mut self, method_name_constant: usize) {
        // Determine if this is an initializer or regular method
        let method_name = self.previous.lexeme;
        let function_type = if method_name == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };

        self.function(function_type);

        // Emit OP_METHOD instruction with the method name constant
        self.emit(Instruction::Method(method_name_constant));
    }

    fn function(&mut self, function_type: FunctionType) {
        let name = self.interner.intern(self.previous.lexeme);

        let new_compiler = Compiler::new_with_name(function_type, name);

        // Swap in the new compiler, storing the old one as enclosing
        let old_compiler = std::mem::replace(&mut self.compiler, new_compiler);
        self.compiler.enclosing = Some(Box::new(old_compiler));

        self.begin_scope();

        // For methods and initializers, set slot 0 to 'this'
        if function_type == FunctionType::Method || function_type == FunctionType::Initializer {
            let this_token = Token {
                kind: TokenType::Identifier,
                lexeme: "this",
                line: 0,
                span: (0, 0),
            };
            self.compiler.locals[0] = Local {
                name: this_token,
                depth: self.compiler.scope_depth,
                is_captured: false,
            };
        }

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

        // Collect upvalue descriptors before restoring enclosing compiler
        let upvalue_descriptors: Vec<UpvalueDescriptor> = self.compiler.upvalues
            .iter()
            .map(|u| UpvalueDescriptor {
                is_local: u.is_local,
                index: u.index,
            })
            .collect();

        // Restore the enclosing compiler
        let enclosing = self.compiler.enclosing.take().expect("No enclosing compiler");
        self.compiler = *enclosing;

        // Emit the closure instruction with upvalue info
        let constant = self.current_chunk().add_constant(Value::CompilerFunction(Rc::new(function)));
        let line = self.previous.line;
        self.current_chunk().write(Instruction::Closure(constant, upvalue_descriptors), line);
    }

    fn end_compiler(&mut self) -> CompilerFunction {
        self.emit_return();

        // Set the upvalue count on the function
        self.compiler.function.upvalue_count = self.compiler.upvalues.len();

        // Take the function out of the compiler
        std::mem::take(&mut self.compiler.function)
    }

    fn emit_return(&mut self) {
        if self.compiler.function_type == FunctionType::Initializer {
            // Initializers return 'this' (which is at slot 0)
            self.emit(Instruction::GetLocal(0));
        } else {
            self.emit(Instruction::Nil);
        }
        self.emit(Instruction::Return);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emit(Instruction::Nil);
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

    fn identifier_constant(&mut self, name: Token) -> usize {
        let name_string = self.interner.intern(name.lexeme);
        self.current_chunk().add_constant(Value::CompilerString(name_string))
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous;

        // Check for duplicate in current scope
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

    /// Creates a synthetic token with the given text (used for "this" and "super")
    fn synthetic_token(&self, text: &'static str) -> Token<'source> {
        Token {
            kind: TokenType::Identifier,
            lexeme: text,
            line: 0,
            span: (0, 0),
        }
    }

    fn add_local(&mut self, name: Token<'source>) {
        let local = Local {
            name,
            depth: -1,
            is_captured: false,
        };
        self.compiler.locals.push(local);
    }

    fn define_variable(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit(Instruction::DefineGlobal(global));
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
            if self.compiler.function_type == FunctionType::Initializer {
                self.error_at_previous("Can't return a value from an initializer.");
            }
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit(Instruction::Return);
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit(Instruction::Print);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit(Instruction::Pop);
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
            if self.compiler.locals.last().unwrap().is_captured {
                // Variable already captured by closure
                self.emit(Instruction::CloseUpvalue);
            } else {
                // Not captured so just pop
                self.emit(Instruction::Pop);
            }
            self.compiler.locals.pop();
        }
    }

    /// Emits a jump instruction with a placeholder offset, returns the index of the instruction
    fn emit_jump(&mut self, instruction: Instruction) -> usize {
        let line = self.previous.line;
        let span = self.previous.span;
        self.current_chunk().write_with_span(instruction, line, span)
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

        self.emit(Instruction::Loop(offset as u16));
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // Jump over the then branch if condition is falsey
        let then_jump = self.emit_jump(Instruction::JumpIfFalse(0));
        self.emit(Instruction::Pop); // Pop condition if true
        self.statement();

        // Jump over the else branch after executing then branch
        let else_jump = self.emit_jump(Instruction::Jump(0));

        self.patch_jump(then_jump);
        self.emit(Instruction::Pop); // Pop condition if false

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
        self.emit(Instruction::Pop); // Pop condition
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit(Instruction::Pop); // Pop condition on exit
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
            self.emit(Instruction::Pop); // Pop condition
        }

        // Increment clause
        if !self.match_token(TokenType::RightParen) {
            // Jump over increment, run body, then come back
            let body_jump = self.emit_jump(Instruction::Jump(0));
            let increment_start = self.current_chunk().code.len();

            self.expression();
            self.emit(Instruction::Pop); // Pop increment result
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
            self.emit(Instruction::Pop); // Pop condition on exit
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
        let (get_op, set_op) = if let Some(slot) = self.resolve_local(&name) {
            (Instruction::GetLocal(slot), Instruction::SetLocal(slot))
        } else if let Some(upvalue) = self.resolve_upvalue(&name) {
            (Instruction::GetUpvalue(upvalue), Instruction::SetUpvalue(upvalue))
        } else {
            let arg = self.identifier_constant(name);
            (Instruction::GetGlobal(arg), Instruction::SetGlobal(arg))
        };

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.emit_at(set_op, name);
        } else {
            self.emit_at(get_op, name);
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

    /// Resolve a variable as an upvalue
    fn resolve_upvalue(&mut self, name: &Token) -> Option<u8> {
        let mut enclosing = self.compiler.enclosing.take()?;

        let result = if let Some(local_idx) = Self::resolve_local_in_compiler(&enclosing, name) {
            // Mark the local as captured
            enclosing.locals[local_idx].is_captured = true;
            // Restore enclosing before adding upvalue
            self.compiler.enclosing = Some(enclosing);
            // Add upvalue to current compiler referencing the local
            Some(self.add_upvalue(local_idx as u8, true))
        } else {
            let current = std::mem::replace(&mut self.compiler, *enclosing);

            // Recursively try to resolve in the enclosing compiler
            let recursive_result = self.resolve_upvalue(name);

            enclosing = Box::new(std::mem::replace(&mut self.compiler, current));

            // Restore enclosing
            self.compiler.enclosing = Some(enclosing);

            if let Some(upvalue_idx) = recursive_result {
                // Found in an outer scope so add upvalue pointing to enclosing's upvalue
                Some(self.add_upvalue(upvalue_idx, false))
            } else {
                None
            }
        };

        result
    }

    /// Resolve a local variable in a specific compiler
    fn resolve_local_in_compiler(compiler: &Compiler, name: &Token) -> Option<usize> {
        for (i, local) in compiler.locals.iter().enumerate().rev() {
            if local.name.lexeme == name.lexeme {
                if local.depth == -1 {
                    return None;
                }
                return Some(i);
            }
        }
        None
    }

    /// Add an upvalue to the current compiler
    fn add_upvalue(&mut self, index: u8, is_local: bool) -> u8 {
        let upvalue_count = self.compiler.upvalues.len();

        // Check if we already have this upvalue
        for (i, upvalue) in self.compiler.upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return i as u8;
            }
        }

        if upvalue_count >= 256 {
            self.error_at_previous("Too many closure variables in function.");
            return 0;
        }

        self.compiler.upvalues.push(Upvalue { index, is_local });
        self.compiler.function.upvalue_count = self.compiler.upvalues.len();
        upvalue_count as u8
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

        let prefix_fn = match prefix_rule {
            Some(f) => f,
            None => {
                self.error_at_previous("Expect expression.");
                return;
            }
        };

        let can_assign = precedence <= Precedence::Assignment;
        prefix_fn(self, can_assign);

        while precedence <= ParseRule::get_rule(self.current.kind).precedence {
            self.advance();
            let infix_rule = ParseRule::get_rule(self.previous.kind).infix;
            if let Some(infix_fn) = infix_rule {
                infix_fn(self, can_assign);
            }
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error_at_previous("Invalid assignment target.");
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message);
    }

    fn error_at_previous(&mut self, message: &str) {
        self.error_at(self.previous, message);
    }

    fn error_at(&mut self, token: Token, message: &str) {
        let error = ParseError::new(message.to_string(), token.span, token.line);
        self.errors.push(LoxError::ParseError(error));
    }
}

/// Compiles source code into bytecode
pub fn compile(source_code: &str, source_name: &str) -> LoxResult<CompilerFunction> {
    let mut parser = Parser::new(source_code, source_name);

    parser.advance();

    while !parser.current.is_token(TokenType::Eof) {
        parser.declaration();
    }

    parser.consume(TokenType::Eof, "Expect end of expression.");

    let function = parser.end_compiler();

    if !parser.errors.is_empty() {
        // Attach source to errors for nice error messages
        let source = NamedSource::new(source_name, source_code.to_string());
        let errors: Vec<LoxError> = parser.errors.into_iter().map(|e| {
            if let LoxError::ParseError(pe) = e {
                LoxError::ParseError(pe.with_source(source.clone()))
            } else {
                e
            }
        }).collect();
        Err(errors)
    } else {
        Ok(function)
    }
}

fn number(parser: &mut Parser, _can_assign: bool) {
    let value: f64 = parser.previous.lexeme.parse().unwrap_or(0.0);
    let line = parser.previous.line;
    let span = parser.previous.span;
    let idx = parser.current_chunk().add_constant(Value::Number(value));
    parser.current_chunk().write_with_span(Instruction::Constant(idx), line, span);
}

fn grouping(parser: &mut Parser, _can_assign: bool) {
    parser.expression();
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

fn call(parser: &mut Parser, _can_assign: bool) {
    let arg_count = parser.argument_list();
    parser.emit(Instruction::Call(arg_count));
}

fn dot(parser: &mut Parser, can_assign: bool) {
    parser.consume(TokenType::Identifier, "Expect property name after '.'.");
    let name = parser.identifier_constant(parser.previous);

    if can_assign && parser.match_token(TokenType::Equal) {
        parser.expression();
        parser.emit(Instruction::SetProperty(name));
    } else if parser.match_token(TokenType::LeftParen) {
        // Method call: use OP_INVOKE
        let mut arg_count = 0u8;
        if !parser.check(TokenType::RightParen) {
            loop {
                parser.expression();
                arg_count += 1;
                if arg_count > u8::MAX {
                    parser.error_at_current("Can't have more than 255 arguments.");
                }
                if !parser.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        parser.consume(TokenType::RightParen, "Expect ')' after arguments.");
        parser.emit(Instruction::Invoke(name, arg_count));
    } else {
        // Property access
        parser.emit(Instruction::GetProperty(name));
    }
}

fn unary(parser: &mut Parser, _can_assign: bool) {
    let operator_type = parser.previous.kind;
    let operator_token = parser.previous;

    // Compile the operand
    parser.parse_precedence(Precedence::Unary);

    // Emit the operator instruction
    match operator_type {
        TokenType::Minus => { parser.emit_at(Instruction::Negate, operator_token); }
        TokenType::Bang => { parser.emit_at(Instruction::Not, operator_token); }
        _ => {}
    }
}

fn binary(parser: &mut Parser, _can_assign: bool) {
    let operator_type = parser.previous.kind;
    let operator_token = parser.previous;

    let rule = ParseRule::get_rule(operator_type);
    parser.parse_precedence(rule.precedence.next());

    match operator_type {
        TokenType::Plus => { parser.emit_at(Instruction::Add, operator_token); }
        TokenType::Minus => { parser.emit_at(Instruction::Sub, operator_token); }
        TokenType::Star => { parser.emit_at(Instruction::Mul, operator_token); }
        TokenType::Slash => { parser.emit_at(Instruction::Div, operator_token); }
        TokenType::BangEqual => {
            parser.emit_at(Instruction::Equal, operator_token);
            parser.emit_at(Instruction::Not, operator_token);
        }
        TokenType::EqualEqual => { parser.emit_at(Instruction::Equal, operator_token); }
        TokenType::Greater => { parser.emit_at(Instruction::Greater, operator_token); }
        TokenType::GreaterEqual => {
            parser.emit_at(Instruction::Less, operator_token);
            parser.emit_at(Instruction::Not, operator_token);
        }
        TokenType::Less => { parser.emit_at(Instruction::Less, operator_token); }
        TokenType::LessEqual => {
            parser.emit_at(Instruction::Greater, operator_token);
            parser.emit_at(Instruction::Not, operator_token);
        }
        _ => {}
    }
}

fn string(parser: &mut Parser, _can_assign: bool) {
    // Remove quotes from string
    let lexeme = parser.previous.lexeme;
    let string_value = &lexeme[1..lexeme.len() - 1];
    let interned = parser.interner.intern(string_value);
    let line = parser.previous.line;
    let span = parser.previous.span;
    let idx = parser.current_chunk().add_constant(Value::CompilerString(interned));
    parser.current_chunk().write_with_span(Instruction::Constant(idx), line, span);
}

fn literal(parser: &mut Parser, _can_assign: bool) {
    match parser.previous.kind {
        TokenType::False => { parser.emit(Instruction::False); }
        TokenType::True => { parser.emit(Instruction::True); }
        TokenType::Nil => { parser.emit(Instruction::Nil); }
        _ => {}
    }
}

fn variable(parser: &mut Parser, can_assign: bool) {
    let name = parser.previous;
    parser.named_variable(name, can_assign);
}

fn this_(parser: &mut Parser, _can_assign: bool) {
    if parser.current_class.is_none() {
        parser.error_at_previous("Can't use 'this' outside of a class.");
        return;
    }

    // Treat 'this' as a variable named "this"
    let token = Token {
        kind: TokenType::Identifier,
        lexeme: "this",
        line: parser.previous.line,
        span: parser.previous.span,
    };
    parser.named_variable(token, false);
}

fn super_(parser: &mut Parser, _can_assign: bool) {
    if parser.current_class.is_none() {
        parser.error_at_previous("Can't use 'super' outside of a class.");
    } else if !parser.current_class.as_ref().unwrap().has_superclass {
        parser.error_at_previous("Can't use 'super' in a class with no superclass.");
    }

    parser.consume(TokenType::Dot, "Expect '.' after 'super'.");
    parser.consume(TokenType::Identifier, "Expect superclass method name.");
    let name = parser.identifier_constant(parser.previous);

    // Load "this" (the instance)
    parser.named_variable(parser.synthetic_token("this"), false);
    
    if parser.match_token(TokenType::LeftParen) {
        // Optimized super invocation: super.method(args)
        let arg_count = parser.argument_list();
        parser.named_variable(parser.synthetic_token("super"), false);
        parser.emit(Instruction::SuperInvoke(name, arg_count));
    } else {
        // Super access: super.method (returns bound method)
        parser.named_variable(parser.synthetic_token("super"), false);
        parser.emit(Instruction::GetSuper(name));
    }
}

fn and_(parser: &mut Parser, _can_assign: bool) {
    // Short-circuit: if left side is false, skip right side
    let end_jump = parser.emit_jump(Instruction::JumpIfFalse(0));

    parser.emit(Instruction::Pop);
    parser.parse_precedence(Precedence::And);

    parser.patch_jump(end_jump);
}

fn or_(parser: &mut Parser, _can_assign: bool) {
    // Short-circuit: if left side is true, skip right side
    let else_jump = parser.emit_jump(Instruction::JumpIfFalse(0));
    let end_jump = parser.emit_jump(Instruction::Jump(0));

    parser.patch_jump(else_jump);
    parser.emit(Instruction::Pop);

    parser.parse_precedence(Precedence::Or);
    parser.patch_jump(end_jump);
}
