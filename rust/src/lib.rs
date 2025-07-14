use crate::chunk::{Instruction, Value};
use crate::error::LoxError;
use crate::vm::VirtualMachine;

mod chunk;
mod error;
mod vm;

pub type LoxResult<T> = Result<T, LoxError>;

#[test]
fn test_program() {
    let mut chunk = chunk::Chunk::new();
    chunk.write_constant(Value::Number(1.2), 0);
    chunk.write(Instruction::Negate, 0);
    chunk.write(Instruction::Return, 0);

    let _ = VirtualMachine::interpret(&chunk);
}
