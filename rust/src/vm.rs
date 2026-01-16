use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    LoxResult,
    chunk::{Chunk, Instruction, Instruction::*, Value},
    error::LoxError,
    object::{LoxFunction, NativeFunction, StringInterner},
};

/// Maximum number of call frames (stack depth)
const FRAMES_MAX: usize = 64;

/// Represents a single function call's execution context
struct CallFrame {
    /// The function being executed
    function: Rc<LoxFunction>,
    /// Instruction pointer into the function's chunk
    ip: usize,
    /// Base index into the VM's value stack for this frame's slots
    slot_offset: usize,
}

impl CallFrame {
    fn new(function: Rc<LoxFunction>, slot_offset: usize) -> Self {
        Self {
            function,
            ip: 0,
            slot_offset,
        }
    }

    /// Read the current instruction and advance ip
    fn read_instruction(&mut self) -> Option<&Instruction> {
        let instruction = self.function.chunk.instruction(self.ip);
        if instruction.is_some() {
            self.ip += 1;
        }
        instruction
    }

    /// Get the current chunk
    fn chunk(&self) -> &Chunk {
        &self.function.chunk
    }
}

/// Virtual machine for executing bytecode instructions
pub struct VirtualMachine {
    /// Call stack of frames
    frames: Vec<CallFrame>,
    /// Stack of values
    stack: Vec<Value>,
    /// String interner for deduplicating strings
    interner: StringInterner,
    /// Global variables table
    globals: HashMap<Rc<String>, Value>,
    /// Indicates if the virtual machine is in debug mode
    debug: bool,
}

impl VirtualMachine {
    pub fn new(debug: bool) -> Self {
        let mut vm = Self {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::new(),
            interner: StringInterner::new(),
            globals: HashMap::new(),
            debug,
        };

        // Define native functions
        vm.define_native("clock", 0, clock_native);
        vm
    }

    /// Define a native function in the global scope
    fn define_native(&mut self, name: &'static str, arity: u8, function: crate::object::NativeFn) {
        let native = NativeFunction::new(name, arity, function);
        let name_rc = self.interner.intern(name);
        self.globals.insert(name_rc, Value::NativeFunction(Rc::new(native)));
    }

    /// Get the current call frame
    fn current_frame(&self) -> &CallFrame {
        self.frames.last().expect("No call frame")
    }

    /// Get the current call frame mutably
    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("No call frame")
    }

    /// Interprets next instruction in the chunk
    /// @return Result indicating if there is more bytecode to interpret
    fn interpret_next(&mut self) -> Result<Option<Value>, LoxError> {
        // Get instruction from current frame
        let instruction = {
            let frame = self.current_frame_mut();
            frame.read_instruction().cloned()
        };

        let instruction = match instruction {
            Some(i) => i,
            None => return Err(LoxError::RuntimeError("Out of bounds instruction.".into())),
        };

        // Print debug information if in debug mode
        if self.debug {
            let frame = self.current_frame();
            println!("IP: {}, Instruction: {:?}", frame.ip - 1, instruction);
            println!("Stack: {:?}", self.stack);
        }

        match instruction {
            Return => {
                let result = self.pop();
                let frame = self.frames.pop().expect("No frame to pop");

                if self.frames.is_empty() {
                    // Finished executing the top-level script
                    return Ok(Some(result));
                }

                // Discard the callee's stack window
                self.stack.truncate(frame.slot_offset);
                self.push(result);
            }
            Pop => {
                self.pop();
            }
            Print => {
                let value = self.pop();
                println!("{}", value);
            }
            DefineGlobal(idx) => {
                let name = self.read_string(idx)?;
                let value = self.pop();
                self.globals.insert(name, value);
            }
            GetGlobal(idx) => {
                let name = self.read_string(idx)?;
                let value = self.globals.get(&name).ok_or_else(|| {
                    LoxError::RuntimeError(format!("Undefined variable '{}'.", name))
                })?;
                self.push(value.clone());
            }
            SetGlobal(idx) => {
                let name = self.read_string(idx)?;
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
                let slot_offset = self.current_frame().slot_offset;
                let value = self.stack[slot_offset + slot].clone();
                self.push(value);
            }
            SetLocal(slot) => {
                let value = self.peek(0).clone();
                let slot_offset = self.current_frame().slot_offset;
                self.stack[slot_offset + slot] = value;
            }
            Constant(idx) => {
                let constant = self
                    .current_frame()
                    .chunk()
                    .constant(idx)
                    .ok_or(LoxError::RuntimeError("No such constant.".into()))?
                    .clone();
                self.push(constant);
            }
            Negate => {
                let value = self.pop();
                match value {
                    Value::Number(num) => self.push(Value::Number(-num)),
                    _ => return Err(LoxError::RuntimeError("Operand must be a number.".into())),
                }
            }
            Add => {
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
                let frame = self.current_frame_mut();
                frame.ip += offset as usize;
            }
            Instruction::JumpIfFalse(offset) => {
                if Self::is_falsey(self.peek(0)) {
                    let frame = self.current_frame_mut();
                    frame.ip += offset as usize;
                }
            }
            Instruction::Loop(offset) => {
                let frame = self.current_frame_mut();
                frame.ip -= offset as usize;
            }
            Instruction::Call(arg_count) => {
                let callee = self.peek(arg_count as usize).clone();
                self.call_value(callee, arg_count)?;
            }
        };

        Ok(None)
    }

    /// Call a value (function or native)
    fn call_value(&mut self, callee: Value, arg_count: u8) -> Result<(), LoxError> {
        match callee {
            Value::Function(function) => self.call(function, arg_count),
            Value::NativeFunction(native) => self.call_native(native, arg_count),
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    /// Call a Lox function
    fn call(&mut self, function: Rc<LoxFunction>, arg_count: u8) -> Result<(), LoxError> {
        // Check arity
        if arg_count != function.arity {
            return Err(self.runtime_error(&format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count
            )));
        }

        // Check for stack overflow
        if self.frames.len() >= FRAMES_MAX {
            return Err(self.runtime_error("Stack overflow."));
        }

        // Create new call frame
        // The slot_offset points to the function object on the stack
        let slot_offset = self.stack.len() - arg_count as usize - 1;
        let frame = CallFrame::new(function, slot_offset);
        self.frames.push(frame);
        Ok(())
    }

    /// Call a native function
    fn call_native(&mut self, native: Rc<NativeFunction>, arg_count: u8) -> Result<(), LoxError> {
        // Check arity
        if arg_count != native.arity {
            return Err(self.runtime_error(&format!(
                "Expected {} arguments but got {}.",
                native.arity, arg_count
            )));
        }

        let args_start = self.stack.len() - arg_count as usize;
        let result = {
            let args = &self.stack[args_start..];
            (native.function)(args).map_err(|e| self.runtime_error(&e))?
        };

        // Pop arguments and function
        self.stack.truncate(args_start - 1);
        self.push(result);
        Ok(())
    }

    /// Generate a runtime error with stack trace
    fn runtime_error(&self, message: &str) -> LoxError {
        let mut trace = String::new();
        trace.push_str(&format!("{}\n", message));

        for frame in self.frames.iter().rev() {
            let function = &frame.function;
            let line = function.chunk.lines.get(frame.ip.saturating_sub(1)).unwrap_or(&0);

            let name = match &function.name {
                Some(n) => format!("{}()", n),
                None => "script".to_string(),
            };
            trace.push_str(&format!("[line {}] in {}\n", line, name));
        }

        LoxError::RuntimeError(trace)
    }

    /// Interprets a compiled function
    /// @param function The compiled function to execute
    /// @return Result indicating if the function was successfully interpreted
    pub fn interpret(function: Rc<LoxFunction>) -> LoxResult<Value> {
        let mut vm = Self::new(false);

        // Push the function onto the stack
        vm.push(Value::Function(function.clone()));

        // Create initial call frame for the script function
        let frame = CallFrame::new(function, 0);
        vm.frames.push(frame);

        loop {
            match vm.interpret_next() {
                Ok(Some(result)) => return Ok(result),
                Ok(None) => continue,
                Err(err) => return Err(vec![err]),
            }
        }
    }

    /// Pushes a value onto the stack
    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    /// Pops a value from the stack
    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    /// Peeks at a value on the stack at the given distance from the top
    pub fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    /// Reads a string constant from the current frame's chunk
    fn read_string(&self, idx: usize) -> Result<Rc<String>, LoxError> {
        match self.current_frame().chunk().constant(idx) {
            Some(Value::String(s)) => Ok(s.clone()),
            _ => Err(LoxError::RuntimeError("Expected string constant.".into())),
        }
    }

    /// Determines if a value is "falsey"
    pub fn is_falsey(value: &Value) -> bool {
        match value {
            Value::Nil => true,
            Value::Bool(b) => !b,
            _ => false,
        }
    }

    /// Compares two values for equality
    pub fn values_equal(left: &Value, right: &Value) -> bool {
        left == right
    }
}

/// Native function: clock() - returns current time in seconds
fn clock_native(_args: &[Value]) -> Result<Value, String> {
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| e.to_string())?;
    Ok(Value::Number(duration.as_secs_f64()))
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
            Value::Function(func) => write!(f, "{}", func),
            Value::NativeFunction(native) => write!(f, "{}", native),
        }
    }
}
