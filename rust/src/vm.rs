use crate::{
    LoxResult,
    chunk::{Chunk, Instruction::*, Value},
    error::LoxError,
};

/// Virtual machine for executing bytecode instructions
pub struct VirtualMachine {
    // The chunk of bytecode instructions and constants
    chunk: Chunk,
    // The current instruction pointer
    ip: usize,
    // Stack of values
    stack: Vec<Value>,

    // Indicates if the virtual machine is in debug mode
    debug: bool,
}

impl VirtualMachine {
    pub fn new_from_chunk(chunk: Chunk, debug: bool) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::new(),
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
                let value = self.pop();
                self.push(value);
                return Ok(false);
            }
            Constant(idx) => {
                // Retrieves a constant value from the chunk and pushes it onto the stack
                let constant = self
                    .chunk
                    .constant(*idx)
                    .ok_or(LoxError::RuntimeError("No such constant.".into()))?;
                self.push(*constant);
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
                // Adds two numbers
                let right = self.pop();
                let left = self.pop();
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left + right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
            }
            Sub => {
                // Subtracts two numbers
                let right = self.pop();
                let left = self.pop();
                match (left, right) {
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
                match (left, right) {
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
                match (left, right) {
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
                self.push(Value::Bool(Self::is_falsey(value)));
            }
            Equal => {
                let right = self.pop();
                let left = self.pop();
                self.push(Value::Bool(Self::values_equal(left, right)));
            }
            Greater => {
                let right = self.pop();
                let left = self.pop();
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Bool(left > right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
            }
            Less => {
                let right = self.pop();
                let left = self.pop();
                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Bool(left < right))
                    }
                    _ => return Err(LoxError::RuntimeError("Operands must be numbers.".into())),
                }
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
                Ok(false) => return Ok(vm.pop()), // Return last value
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

    /// Determines if a value is "falsey" (i.e., evaluates to false in a boolean context)
    /// @param value The value to check
    /// @return True if the value is falsey, false otherwise
    pub fn is_falsey(value: Value) -> bool {
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
    pub fn values_equal(left: Value, right: Value) -> bool {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}
