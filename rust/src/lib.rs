mod chunk;
mod compiler;
mod error;
mod scanner;
mod vm;

pub use crate::error::LoxError;
pub use crate::vm::VirtualMachine;

pub type LoxResult<T> = Result<T, LoxError>;

#[test]
fn test_program() {
    use crate::chunk::{Instruction, Value};

    let mut chunk = chunk::Chunk::new();
    chunk.write_constant(Value::Number(1.2), 0);
    chunk.write(Instruction::Negate, 0);
    chunk.write(Instruction::Return, 0);

    let _ = VirtualMachine::interpret(chunk);
}
