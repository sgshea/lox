use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use miette::NamedSource;

use crate::{
    LoxResult,
    chunk::{Chunk, Instruction, Instruction::*, Value},
    error::{LoxError, RuntimeError},
    object::{LoxClosure, LoxFunction, LoxUpvalue, NativeFunction, StringInterner},
};

/// Maximum number of call frames (stack depth)
const FRAMES_MAX: usize = 64;

/// Represents a single function call's execution context
struct CallFrame {
    /// The closure being executed
    closure: Rc<LoxClosure>,
    /// Instruction pointer into the closure's function's chunk
    ip: usize,
    /// Base index into the VM's value stack for this frame's slots
    slot_offset: usize,
}

impl CallFrame {
    fn new(closure: Rc<LoxClosure>, slot_offset: usize) -> Self {
        Self {
            closure,
            ip: 0,
            slot_offset,
        }
    }

    /// Read the current instruction and advance ip
    fn read_instruction(&mut self) -> Option<&Instruction> {
        let instruction = self.closure.function.chunk.instruction(self.ip);
        if instruction.is_some() {
            self.ip += 1;
        }
        instruction
    }

    /// Get the current chunk
    fn chunk(&self) -> &Chunk {
        &self.closure.function.chunk
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
    /// Head of the open upvalues linked list (sorted by stack index, descending)
    open_upvalues: Option<Rc<RefCell<LoxUpvalue>>>,
    /// Indicates if the virtual machine is in debug mode
    debug: bool,
    /// Source code for error reporting
    source: Option<NamedSource<String>>,
}

impl VirtualMachine {
    pub fn new(debug: bool) -> Self {
        let mut vm = Self {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::new(),
            interner: StringInterner::new(),
            globals: HashMap::new(),
            open_upvalues: None,
            debug,
            source: None,
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
            None => return Err(self.runtime_error("Out of bounds instruction.")),
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

                // Close any remaining open upvalues for this frame
                self.close_upvalues(frame.slot_offset);

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
                    self.runtime_error(&format!("Undefined variable '{}'.", name))
                })?;
                self.push(value.clone());
            }
            SetGlobal(idx) => {
                let name = self.read_string(idx)?;
                if !self.globals.contains_key(&name) {
                    return Err(self.runtime_error(&format!(
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
            GetUpvalue(slot) => {
                let upvalue = self.current_frame().closure.upvalues[slot as usize].clone();
                let value = {
                    let uv = upvalue.borrow();
                    if let Some(stack_idx) = uv.location {
                        // Open upvalue - read from stack
                        self.stack[stack_idx].clone()
                    } else {
                        // Closed upvalue - read stored value
                        uv.closed.clone()
                    }
                };
                self.push(value);
            }
            SetUpvalue(slot) => {
                let value = self.peek(0).clone();
                let upvalue = self.current_frame().closure.upvalues[slot as usize].clone();
                let mut uv = upvalue.borrow_mut();
                if let Some(stack_idx) = uv.location {
                    // Open upvalue - write to stack
                    self.stack[stack_idx] = value;
                } else {
                    // Closed upvalue - write to stored value
                    uv.closed = value;
                }
            }
            CloseUpvalue => {
                let top = self.stack.len() - 1;
                self.close_upvalues(top);
                self.pop();
            }
            Constant(idx) => {
                let constant = self
                    .current_frame()
                    .chunk()
                    .constant(idx)
                    .ok_or_else(|| self.runtime_error("No such constant."))?
                    .clone();
                self.push(constant);
            }
            Closure(idx, ref upvalue_descs) => {
                let function = match self.current_frame().chunk().constant(idx) {
                    Some(Value::Function(f)) => f.clone(),
                    _ => return Err(self.runtime_error("Expected function constant.")),
                };

                let mut closure = LoxClosure::new(function);

                // Capture upvalues
                for desc in upvalue_descs {
                    let upvalue = if desc.is_local {
                        // Capture local from enclosing function
                        let slot_offset = self.current_frame().slot_offset;
                        let stack_index = slot_offset + desc.index as usize;
                        self.capture_upvalue(stack_index)
                    } else {
                        // Capture upvalue from enclosing closure
                        self.current_frame().closure.upvalues[desc.index as usize].clone()
                    };
                    closure.upvalues.push(upvalue);
                }

                self.push(Value::Closure(Rc::new(closure)));
            }
            Negate => {
                let value = self.pop();
                match value {
                    Value::Number(num) => self.push(Value::Number(-num)),
                    _ => return Err(self.runtime_error("Operand must be a number.")),
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
                    _ => return Err(self.runtime_error("Operands must be two numbers or two strings.")),
                }
            }
            Sub => {
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left - right))
                    }
                    _ => return Err(self.runtime_error("Operands must be numbers.")),
                }
            }
            Mul => {
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left * right))
                    }
                    _ => return Err(self.runtime_error("Operands must be numbers.")),
                }
            }
            Div => {
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Number(left / right))
                    }
                    _ => return Err(self.runtime_error("Operands must be numbers.")),
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
                    _ => return Err(self.runtime_error("Operands must be numbers.")),
                }
            }
            Less => {
                let right = self.pop();
                let left = self.pop();
                match (&left, &right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.push(Value::Bool(left < right))
                    }
                    _ => return Err(self.runtime_error("Operands must be numbers.")),
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

    /// Captures a local variable, reusing existing upvalue if one exists
    fn capture_upvalue(&mut self, stack_index: usize) -> Rc<RefCell<LoxUpvalue>> {
        // Walk the open upvalues list looking for one that captures this slot
        let mut prev: Option<Rc<RefCell<LoxUpvalue>>> = None;
        let mut current = self.open_upvalues.clone();

        loop {
            let upvalue_rc = match current {
                Some(ref rc) => rc.clone(),
                None => break,
            };

            let (loc, next) = {
                let upvalue = upvalue_rc.borrow();
                (upvalue.location, upvalue.next.clone())
            };

            if let Some(l) = loc {
                if l == stack_index {
                    // Found existing upvalue for this slot
                    return upvalue_rc;
                }
                if l < stack_index {
                    // Passed the slot, not found
                    break;
                }
            }

            prev = Some(upvalue_rc);
            current = next;
        }

        // Create new upvalue
        let new_upvalue = Rc::new(RefCell::new(LoxUpvalue::new(stack_index)));

        // Insert into list
        new_upvalue.borrow_mut().next = current;

        if let Some(prev_rc) = prev {
            prev_rc.borrow_mut().next = Some(new_upvalue.clone());
        } else {
            self.open_upvalues = Some(new_upvalue.clone());
        }

        new_upvalue
    }

    /// Closes all upvalues pointing to slots at or above the given index
    fn close_upvalues(&mut self, last: usize) {
        while let Some(ref upvalue_rc) = self.open_upvalues.clone() {
            let mut upvalue = upvalue_rc.borrow_mut();

            if let Some(loc) = upvalue.location {
                if loc < last {
                    break;
                }

                // Close this upvalue
                let value = self.stack[loc].clone();
                upvalue.close(value);

                // Remove from open list
                self.open_upvalues = upvalue.next.take();
            } else {
                break;
            }
        }
    }

    /// Call a value (function or native)
    fn call_value(&mut self, callee: Value, arg_count: u8) -> Result<(), LoxError> {
        match callee {
            Value::Closure(closure) => self.call(closure, arg_count),
            Value::NativeFunction(native) => self.call_native(native, arg_count),
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    /// Call a Lox closure
    fn call(&mut self, closure: Rc<LoxClosure>, arg_count: u8) -> Result<(), LoxError> {
        // Check arity
        if arg_count != closure.function.arity {
            return Err(self.runtime_error(&format!(
                "Expected {} arguments but got {}.",
                closure.function.arity, arg_count
            )));
        }

        // Check for stack overflow
        if self.frames.len() >= FRAMES_MAX {
            return Err(self.runtime_error("Stack overflow."));
        }

        // Create new call frame
        // The slot_offset points to the closure object on the stack
        let slot_offset = self.stack.len() - arg_count as usize - 1;
        let frame = CallFrame::new(closure, slot_offset);
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

    /// Generate a runtime error with stack trace and source information
    fn runtime_error(&self, message: &str) -> LoxError {
        // Get the current instruction's location info
        let frame = self.current_frame();
        let ip = frame.ip.saturating_sub(1);
        let line = *frame.chunk().lines.get(ip).unwrap_or(&0);
        let span = frame.chunk().span(ip).unwrap_or((0, 0));

        // Build stack trace
        let mut stack_trace = String::new();
        stack_trace.push_str("Stack trace:\n");
        for frame in self.frames.iter().rev() {
            let function = &frame.closure.function;
            let frame_line = function.chunk.lines.get(frame.ip.saturating_sub(1)).unwrap_or(&0);

            let name = match &function.name {
                Some(n) => format!("{}()", n),
                None => "script".to_string(),
            };
            stack_trace.push_str(&format!("  [line {}] in {}\n", frame_line, name));
        }

        // Create runtime error with source if available
        let error = RuntimeError::new(message, span, line)
            .with_stack_trace(stack_trace);

        // Attach source if available
        let error = if let Some(ref source) = self.source {
            error.with_source(source.clone())
        } else {
            error
        };

        LoxError::RuntimeError(error)
    }

    /// Interprets a compiled function
    /// @param function The compiled function to execute
    /// @return Result indicating if the function was successfully interpreted
    pub fn interpret(function: LoxFunction) -> LoxResult<Value> {
        let mut vm = Self::new(false);

        // Store source for error reporting
        vm.source = function.source.clone();

        // Wrap the function in a closure
        let function_rc = Rc::new(function);
        let closure = Rc::new(LoxClosure::new(function_rc));

        // Push the closure onto the stack
        vm.push(Value::Closure(closure.clone()));

        // Create initial call frame for the script closure
        let frame = CallFrame::new(closure, 0);
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
            _ => Err(self.runtime_error("Expected string constant.")),
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
            Value::Closure(closure) => write!(f, "{}", closure),
            Value::NativeFunction(native) => write!(f, "{}", native),
        }
    }
}
