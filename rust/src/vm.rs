use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use crate::{
    LoxResult,
    chunk::{Chunk, Instruction, Instruction::*, Value},
    error::LoxError,
    object::StringInterner,
};

/// Virtual machine for executing bytecode instructions
pub struct VirtualMachine {
    // The chunk of bytecode instructions and constants
    chunk: Chunk,
    // The current instruction pointer
    ip: usize,
    // Stack of values
    stack: Vec<Value>,
    // String interner for deduplicating strings
    interner: StringInterner,
    // Global variables table
    globals: HashMap<Rc<String>, Value>,

    // Indicates if the virtual machine is in debug mode
    debug: bool,
}

impl VirtualMachine {
    pub fn new_from_chunk(chunk: Chunk, debug: bool) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::new(),
            interner: StringInterner::new(),
            globals: HashMap::new(),
            debug,
        }
    }

    /// Interprets next instruction in the chunk
    /// @return Result indicating if there is more bytecode to interpret
    fn interpret_next(&mut self) -> Result<bool, LoxError> {
        // Get instruction or return error
        let instruction = self
            .chunk
            .instruction(self.ip)
            .ok_or(LoxError::RuntimeError("Out of bounds instruction.".into()))?;
        self.ip += 1;

        // Print debug information if in debug mode
        if self.debug {
            println!("IP: {}, Instruction: {:?}", self.ip - 1, instruction);
            // Current stack contents
            println!("Stack: {:?}", self.stack);
        }

        match instruction {
            Return => {
                return Ok(false);
            }
            Pop => {
                self.pop();
            }
            Print => {
                let value = self.pop();
                println!("{}", value);
            }
            DefineGlobal(idx) => {
                let name = self.read_string(*idx)?;
                let value = self.pop();
                self.globals.insert(name, value);
            }
            GetGlobal(idx) => {
                let name = self.read_string(*idx)?;
                let value = self.globals.get(&name).ok_or_else(|| {
                    LoxError::RuntimeError(format!("Undefined variable '{}'.", name))
                })?;
                self.push(value.clone());
            }
            SetGlobal(idx) => {
                let name = self.read_string(*idx)?;
                if !self.globals.contains_key(&name) {
                    return Err(LoxError::RuntimeError(format!(
                        "Undefined variable '{}'.",
                        name
                    )));
                }
                let value = self.peek(0).clone();
                self.globals.insert(name, value);
            }
            GetLocal(slot) => {
                let value = self.stack[*slot].clone();
                self.push(value);
            }
            SetLocal(slot) => {
                let value = self.peek(0).clone();
                self.stack[*slot] = value;
            }
            Constant(idx) => {
                // Retrieves a constant value from the chunk and pushes it onto the stack
                let constant = self
                    .chunk
                    .constant(*idx)
                    .ok_or(LoxError::RuntimeError("No such constant.".into()))?;
                self.push(constant.clone());
            }
            Negate => {
                // Negates a number
                let value = self.pop();
                match value {
                    Value::Number(num) => self.push(Value::Number(-num)),
                    _ => return Err(LoxError::RuntimeError("Operand must be a number.".into())),
                }
            }
            Add => {
                // Adds two numbers or concatenates two strings
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left + right))
                    }
                    (Value::String(left), Value::String(right)) => {
                        let concatenated = format!("{}{}", left, right);
                        let interned = self.interner.intern(&concatenated);
                        self.push(Value::String(interned));
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be two numbers or two strings.".into())),
                }
            }
            Sub => {
                // Subtracts two numbers
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left - right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
            }
            Mul => {
                // Multiplies two numbers
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left * right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
            }
            Div => {
                // Divides two numbers
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left / right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
            }
            Nil => self.push(Value::Nil),
            True => self.push(Value::Bool(true)),
            False => self.push(Value::Bool(false)),
            Not => {
                let value = self.pop();
                self.push(Value::Bool(Self::is_falsey(&value)));
            }
            Equal => {
                let right = self.pop();
                let left = self.pop();
                self.push(Value::Bool(Self::values_equal(&left, &right)));
            }
            Greater => {
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Bool(left > right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
            }
            Less => {
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Bool(left < right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
            }
            Instruction::Jump(offset) => {
                self.ip += *offset as usize;
            }
            Instruction::JumpIfFalse(offset) => {
                if Self::is_falsey(self.peek(0)) {
                    self.ip += *offset as usize;
                }
            }
            Instruction::Loop(offset) => {
                self.ip -= *offset as usize;
            }
        };

        if self.ip > self.chunk.code.len() {
            Ok(false)
        } else {
            Ok(true)
        }
    }

    /// Interprets the entire chunk of bytecode instructions
    /// @param chunk The chunk of bytecode instructions and constants
    /// @return Result indicating if the chunk was successfully interpreted
    pub fn interpret(chunk: Chunk) -> LoxResult<Value> {
        let mut vm = Self::new_from_chunk(chunk, false);
        loop {
            match vm.interpret_next() {
                Ok(true) => continue,
                Ok(false) => {
                    // Return top of stack if available, otherwise nil
                    return Ok(vm.stack.pop().unwrap_or(Value::Nil));
                }
                Err(err) => return Err(vec![err]),
            }
        }
    }

    /// Pushes a value onto the stack
    /// @param value The value to push onto the stack
    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    /// Pops a value from the stack
    /// @return The value popped from the stack
    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    /// Peeks at a value on the stack at the given distance from the top
    /// @param distance The distance from the top of the stack
    /// @return A reference to the value at that position
    pub fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    /// Reads a string constant from the chunk
    /// @param idx The index of the constant
    /// @return The string value
    fn read_string(&self, idx: usize) -> Result<Rc<String>, LoxError> {
        match self.chunk.constant(idx) {
            Some(Value::String(s)) => Ok(s.clone()),
            _ => Err(LoxError::RuntimeError("Expected string constant.".into())),
        }
    }

    /// Determines if a value is "falsey" (i.e., evaluates to false in a boolean context)
    /// @param value The value to check
    /// @return True if the value is falsey, false otherwise
    pub fn is_falsey(value: &Value) -> bool {
        match value {
            Value::Nil => true,
            Value::Bool(b) => !b,
            _ => false,
        }
    }

    /// Compares two values for equality
    /// @param left The left value
    /// @param right The right value
    /// @return True if the values are equal, false otherwise
    pub fn values_equal(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::String(l), Value::String(r)) => l == r,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => {
                // Print numbers without trailing .0 if they're integers
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}
