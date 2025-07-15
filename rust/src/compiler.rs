use crate::{LoxError, LoxResult, chunk::Chunk, scanner::Token};

pub struct Parser<'source> {
    pub current: Token<'source>,
    pub previous: Token<'source>,
}

pub fn compile(source: &str, chunk: &mut Chunk) -> LoxResult<()> {
    Ok(())
}
