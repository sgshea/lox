mod chunk;
mod compiler;
mod error;
mod scanner;
mod vm;

use miette::Result;

use crate::chunk::Chunk;
pub use crate::chunk::Value;
use crate::compiler::compile;
pub use crate::error::LoxError;
use crate::vm::VirtualMachine;

/// Vector error type for the language
pub type LoxResult<T> = Result<T, Vec<LoxError>>;

/// Main way to run the language
/// @param source_code - The source code to interpret
/// @param source_name - The name of the source file
pub fn interpret(source_code: &str, source_name: &str) -> LoxResult<Value> {
    // Create chunk
    let mut chunk = Chunk::new();
    // Compile source into chunk bytecode
    let compile_result = compile(source_code, source_name, &mut chunk);

    match compile_result {
        Ok(_) => {
            // Create vm and run
            VirtualMachine::interpret(chunk)
        }
        Err(err) => {
            // Return errors
            Err(err)
        }
    }
}

#[test]
fn test_program() {
    use crate::chunk::{Instruction, Value};

    let mut chunk = chunk::Chunk::new();
    chunk.write_constant(Value::Number(1.2), 0);
    chunk.write(Instruction::Negate, 0);
    chunk.write(Instruction::Return, 0);

    let _ = VirtualMachine::interpret(chunk);
}
