use std::fmt::Debug;
use std::rc::Rc;

use crate::object::{LoxFunction, NativeFunction};

/// Op code instructions
#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    // Signal returning
    Return,
    // Constant OpCode which stores operand that is the index of the constant in the constant array
    Constant(usize),
    // Negates a number
    Negate,
    // Binary operations
    Add,
    Sub,
    Mul,
    Div,
    // Literal values
    Nil,
    True,
    False,
    Not,
    // Equality operations
    Equal,
    Greater,
    Less,
    // Stack operations
    Pop,
    // Print statement
    Print,
    // Global variable operations
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    // Local variable operations
    GetLocal(usize),
    SetLocal(usize),
    // Control flow instructions
    Jump(u16),        // Unconditional forward jump by offset
    JumpIfFalse(u16), // Jump if top of stack is falsey (does not pop)
    Loop(u16),        // Unconditional backward jump by offset
    // Function call instruction
    Call(u8),         // Call a function with the given number of arguments
}

/// Values of the language
#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    String(Rc<String>),
    Function(Rc<LoxFunction>),
    NativeFunction(Rc<NativeFunction>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::String(a), Value::String(b)) => a == b,
            // Functions are equal only if they are the same object (pointer equality)
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            (Value::NativeFunction(a), Value::NativeFunction(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

/// Chunk of bytecode instructions and constants
pub struct Chunk {
    // The code instructions from the input
    pub code: Vec<Instruction>,
    // Stored constant values
    pub constants: Vec<Value>,
    // Line numbers of the code instructions
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    // Write an instruction to the chunk
    // @param op_code - The instruction to write
    // @param line - The line number of the instruction
    // @return The index of the instruction in the code vector
    pub fn write(&mut self, op_code: Instruction, line: usize) -> usize {
        self.code.push(op_code);
        self.lines.push(line);
        self.code.len() - 1
    }

    // Add a constant to the chunk
    // @param constant - The constant to add
    // @return The index of the constant in the constant vector
    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    // Write a constant to the chunk
    // @param constant - The constant to write
    // @param line - The line number of the constant
    // @return The index of the constant in the constant vector
    pub fn write_constant(&mut self, constant: Value, line: usize) -> usize {
        let constant_index = self.add_constant(constant);
        self.write(Instruction::Constant(constant_index), line)
    }

    // Reads a constant from the chunk
    // @param index - The index of the constant
    // @return The constant at the given index, or None if the index is out of bounds
    pub fn constant(&self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }

    // Get the instruction at the given index
    // @param index - The index of the instruction
    // @return The instruction at the given index, or None if the index is out of bounds
    pub fn instruction(&self, index: usize) -> Option<&Instruction> {
        self.code.get(index)
    }

    /// Emits a sequence of instructions to the chunk at the given line.
    /// @param chunk The chunk to write the instructions to
    /// @param line The line number to associate with the instructions
    /// @param instructions The instructions to emit
    pub fn emit_instructions(&mut self, line: usize, instructions: &[Instruction]) {
        for instruction in instructions {
            self.write(*instruction, line);
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Chunk:")?;
        for (i, op_code) in self.code.iter().enumerate() {
            let line = self.lines.get(i).unwrap_or(&0);
            match op_code {
                Instruction::Constant(index) => {
                    writeln!(f, "{:04} {:4} {:?} {:?}", i, line, op_code, self.constants.get(*index))?;
                }
                Instruction::DefineGlobal(index) | Instruction::GetGlobal(index) | Instruction::SetGlobal(index) => {
                    writeln!(f, "{:04} {:4} {:?} {:?}", i, line, op_code, self.constants.get(*index))?;
                }
                Instruction::Call(arg_count) => {
                    writeln!(f, "{:04} {:4} {:?} ({} args)", i, line, op_code, arg_count)?;
                }
                _ => writeln!(f, "{:04} {:4} {:?}", i, line, op_code)?,
            }
        }
        Ok(())
    }
}
