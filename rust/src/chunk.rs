use std::fmt::Debug;

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
}

/// Values of the language
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Value {
    Number(f64),
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
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Chunk:")?;
        for op_code in &self.code {
            match op_code {
                Instruction::Constant(index) => {
                    writeln!(f, "{:?} {:?}", op_code, self.constants.get(*index as usize))?;
                }
                _ => writeln!(f, "{:?}", op_code)?,
            }
        }
        Ok(())
    }
}
