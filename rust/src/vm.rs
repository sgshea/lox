use std::collections::HashMap;
use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};

use miette::NamedSource;

use crate::{
    LoxResult,
    chunk::{Chunk, Instruction, Instruction::*, Value},
    error::{LoxError, RuntimeError},
    gc::{Gc, GcError, GcRef},
    object::{CompilerFunction, LoxClosure, LoxFunction, LoxUpvalue, NativeFunction},
};

/// Maximum number of call frames (stack depth)
const FRAMES_MAX: usize = 64;

/// Represents a single function call's execution context
struct CallFrame {
    /// The closure being executed
    closure: GcRef<LoxClosure>,
    /// Instruction pointer into the closure's function's chunk
    ip: usize,
    /// Base index into the VM's value stack for this frame's slots
    slot_offset: usize,
}

impl CallFrame {
    fn new(closure: GcRef<LoxClosure>, slot_offset: usize) -> Self {
        Self {
            closure,
            ip: 0,
            slot_offset,
        }
    }

    /// Get the current chunk
    fn chunk<'a>(&self, gc: &'a Gc) -> &'a Chunk {
        let closure = gc.deref(self.closure);
        let function = gc.deref(closure.function);
        &function.chunk
    }
}

/// Virtual machine for executing bytecode instructions
pub struct VirtualMachine {
    /// Garbage collector
    gc: Gc,
    /// Call stack of frames
    frames: Vec<CallFrame>,
    /// Stack of values
    stack: Vec<Value>,
    /// Global variables table
    globals: HashMap<GcRef<String>, Value>,
    /// Head of the open upvalues linked list (sorted by stack index, descending)
    open_upvalues: Option<GcRef<LoxUpvalue>>,
    /// Indicates if the virtual machine is in debug mode
    debug: bool,
    /// Source for error reporting
    source: Option<NamedSource<String>>,
}

impl VirtualMachine {
    pub fn new(debug: bool) -> Self {
        let mut vm = Self {
            gc: Gc::new(),
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::new(),
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
        let name_gc = self.gc.intern(name).expect("Failed to intern native function name");
        let native_gc = self.gc.alloc(native).expect("Failed to allocate native function");
        self.globals.insert(name_gc, Value::NativeFunction(native_gc));
    }

    /// Check if GC should run and trigger collection if needed
    fn maybe_collect(&mut self) {
        if self.gc.should_gc() {
            self.mark_roots();
            self.gc.collect_garbage();
        }
    }

    /// Mark all roots for garbage collection
    fn mark_roots(&mut self) {
        // Mark stack values
        for value in &self.stack {
            self.gc.mark_value(value);
        }
        
        // Mark globals table
        for (&key, value) in &self.globals {
            self.gc.mark_object(key);
            self.gc.mark_value(value);
        }
        
        // Mark call frames (closures)
        for frame in &self.frames {
            self.gc.mark_object(frame.closure);
        }
        
        // Mark open upvalues chain
        let mut current = self.open_upvalues;
        while let Some(uv_ref) = current {
            self.gc.mark_object(uv_ref);
            let upvalue = self.gc.deref(uv_ref);
            current = upvalue.next;
        }
    }

    /// Format a value for display, dereferencing GC references
    fn format_value(&self, value: &Value) -> String {
        match value {
            Value::Number(n) => {
                // Print numbers without trailing .0 if they're integers
                if n.fract() == 0.0 {
                    format!("{}", *n as i64)
                } else {
                    format!("{}", n)
                }
            }
            Value::Bool(b) => format!("{}", b),
            Value::Nil => "nil".to_string(),
            Value::String(gc_ref) => {
                let s = self.gc.deref(*gc_ref);
                format!("{}", s)
            }
            Value::Function(gc_ref) => {
                let func = self.gc.deref(*gc_ref);
                if let Some(name_ref) = func.name {
                    let name = self.gc.deref(name_ref);
                    format!("<fn {}>", name)
                } else {
                    "<fn script>".to_string()
                }
            }
            Value::Closure(gc_ref) => {
                let closure = self.gc.deref(*gc_ref);
                let func = self.gc.deref(closure.function);
                if let Some(name_ref) = func.name {
                    let name = self.gc.deref(name_ref);
                    format!("<fn {}>", name)
                } else {
                    "<fn script>".to_string()
                }
            }
            Value::NativeFunction(gc_ref) => {
                let native = self.gc.deref(*gc_ref);
                format!("<native fn {}>", native.name)
            }
            Value::Class(gc_ref) => {
                let class = self.gc.deref(*gc_ref);
                let name = self.gc.deref(class.name);
                format!("{}", name)
            }
            Value::Instance(gc_ref) => {
                let instance = self.gc.deref(*gc_ref);
                let class = self.gc.deref(instance.klass);
                let name = self.gc.deref(class.name);
                format!("{} instance", name)
            }
            Value::CompilerString(s) => format!("{}", s),
            Value::CompilerFunction(_) => "<fn>".to_string(),
        }
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
        // We need to carefully manage borrows here
        let instruction = {
            // Get the closure and IP from the frame
            let frame = self.current_frame();
            let closure_ref = frame.closure;
            let ip = frame.ip;
            
            // Now read the instruction using immutable borrows
            let closure = self.gc.deref(closure_ref);
            let function = self.gc.deref(closure.function);
            let instruction = function.chunk.instruction(ip).cloned();
            
            // Advance IP if we got an instruction
            if instruction.is_some() {
                let frame_mut = self.current_frame_mut();
                frame_mut.ip += 1;
            }
            
            instruction
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
                let formatted = self.format_value(&value);
                println!("{}", formatted);
            }
            DefineGlobal(idx) => {
                let name = self.read_string(idx)?;
                let value = self.pop();
                self.globals.insert(name, value);
            }
            GetGlobal(idx) => {
                let name = self.read_string(idx)?;
                let value = self.globals.get(&name).ok_or_else(|| {
                    let name_str = self.gc.deref(name);
                    self.runtime_error(&format!("Undefined variable '{}'.", name_str))
                })?;
                self.push(value.clone());
            }
            SetGlobal(idx) => {
                let name = self.read_string(idx)?;
                if !self.globals.contains_key(&name) {
                    let name_str = self.gc.deref(name);
                    return Err(self.runtime_error(&format!(
                        "Undefined variable '{}'.",
                        name_str
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
                let frame = self.current_frame();
                let closure = self.gc.deref(frame.closure);
                let upvalue_ref = closure.upvalues[slot as usize];
                let upvalue = self.gc.deref(upvalue_ref);
                
                let value = if let Some(stack_idx) = upvalue.location {
                    // Open upvalue - read from stack
                    self.stack[stack_idx].clone()
                } else {
                    // Closed upvalue - read stored value
                    upvalue.closed.clone()
                };
                self.push(value);
            }
            SetUpvalue(slot) => {
                let value = self.peek(0).clone();
                let frame = self.current_frame();
                let closure = self.gc.deref(frame.closure);
                let upvalue_ref = closure.upvalues[slot as usize];
                let upvalue = self.gc.deref_mut(upvalue_ref);
                
                if let Some(stack_idx) = upvalue.location {
                    // Open upvalue - write to stack
                    self.stack[stack_idx] = value;
                } else {
                    // Closed upvalue - write to stored value
                    upvalue.closed = value;
                }
            }
            CloseUpvalue => {
                let top = self.stack.len() - 1;
                self.close_upvalues(top);
                self.pop();
            }
            Constant(idx) => {
                let frame = self.current_frame();
                let chunk = frame.chunk(&self.gc);
                let constant = chunk.constant(idx)
                    .ok_or_else(|| self.runtime_error("No such constant."))?;
                self.push(constant.clone());
            }
            Closure(idx, ref upvalue_descs) => {
                // Trigger GC before allocation
                self.maybe_collect();
                
                let frame = self.current_frame();
                let chunk = frame.chunk(&self.gc);
                let function = match chunk.constant(idx) {
                    Some(Value::Function(f)) => *f,
                    _ => return Err(self.runtime_error("Expected function constant.")),
                };

                let mut closure = LoxClosure::new(function);

                // Capture upvalues
                for desc in upvalue_descs {
                    let upvalue = if desc.is_local {
                        // Capture local from enclosing function
                        let slot_offset = self.current_frame().slot_offset;
                        let stack_index = slot_offset + desc.index as usize;
                        self.capture_upvalue(stack_index)?
                    } else {
                        // Capture upvalue from enclosing closure
                        let frame = self.current_frame();
                        let closure_ref = self.gc.deref(frame.closure);
                        closure_ref.upvalues[desc.index as usize]
                    };
                    closure.upvalues.push(upvalue);
                }

                let closure_ref = self.gc.alloc(closure)
                    .map_err(|_| self.runtime_error("Out of memory."))?;
                self.push(Value::Closure(closure_ref));
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
                        // Trigger GC before allocation
                        self.maybe_collect();
                        
                        let left_str = self.gc.deref(*left);
                        let right_str = self.gc.deref(*right);
                        let concatenated = format!("{}{}", left_str, right_str);
                        let interned = self.gc.intern(&concatenated)
                            .map_err(|_| self.runtime_error("Out of memory."))?;
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
            Instruction::Class(idx) => {
                self.maybe_collect();
                let name = self.read_string(idx)?;
                let class = crate::object::LoxClass::new(name);
                let class_ref = self.gc.alloc(class)
                    .map_err(|_| self.runtime_error("Out of memory."))?;
                self.push(Value::Class(class_ref));
            }
            Instruction::GetProperty(idx) => {
                if let Value::Instance(instance_ref) = self.peek(0).clone() {
                    let name = self.read_string(idx)?;
                    let instance = self.gc.deref(instance_ref);
                    
                    if let Some(value) = instance.fields.get(&name) {
                        let value = value.clone();
                        self.pop();
                        self.push(value);
                    } else {
                        let name_str = self.gc.deref(name);
                        return Err(self.runtime_error(&format!(
                            "Undefined property '{}'.", name_str
                        )));
                    }
                } else {
                    return Err(self.runtime_error("Only instances have properties."));
                }
            }
            Instruction::SetProperty(idx) => {
                if let Value::Instance(instance_ref) = self.peek(1).clone() {
                    let name = self.read_string(idx)?;
                    let value = self.peek(0).clone();
                    
                    let instance = self.gc.deref_mut(instance_ref);
                    instance.fields.insert(name, value.clone());
                    
                    let value = self.pop();
                    self.pop();
                    self.push(value);
                } else {
                    return Err(self.runtime_error("Only instances have fields."));
                }
            }
        };

        Ok(None)
    }

    /// Captures a local variable, reusing existing upvalue if one exists
    fn capture_upvalue(&mut self, stack_index: usize) -> Result<GcRef<LoxUpvalue>, LoxError> {
        // Walk the open upvalues list looking for one that captures this slot
        let mut prev: Option<GcRef<LoxUpvalue>> = None;
        let mut current = self.open_upvalues;

        loop {
            let upvalue_ref = match current {
                Some(ref_val) => ref_val,
                None => break,
            };

            let upvalue = self.gc.deref(upvalue_ref);
            let loc = upvalue.location;
            let next = upvalue.next;

            if let Some(l) = loc {
                if l == stack_index {
                    // Found existing upvalue for this slot
                    return Ok(upvalue_ref);
                }
                if l < stack_index {
                    // Passed the slot, not found
                    break;
                }
            }

            prev = Some(upvalue_ref);
            current = next;
        }

        // Trigger GC before allocation
        self.maybe_collect();

        // Create new upvalue
        let new_upvalue = LoxUpvalue::new(stack_index);
        let new_upvalue_ref = self.gc.alloc(new_upvalue)
            .map_err(|_| self.runtime_error("Out of memory."))?;

        // Insert into list
        self.gc.deref_mut(new_upvalue_ref).next = current;

        if let Some(prev_ref) = prev {
            self.gc.deref_mut(prev_ref).next = Some(new_upvalue_ref);
        } else {
            self.open_upvalues = Some(new_upvalue_ref);
        }

        Ok(new_upvalue_ref)
    }

    /// Closes all upvalues pointing to slots at or above the given index
    fn close_upvalues(&mut self, last: usize) {
        while let Some(upvalue_ref) = self.open_upvalues {
            let upvalue = self.gc.deref(upvalue_ref);
            
            if let Some(loc) = upvalue.location {
                if loc < last {
                    break;
                }

                // Close this upvalue
                let value = self.stack[loc].clone();
                let next = upvalue.next;
                
                // Now mutate
                let upvalue_mut = self.gc.deref_mut(upvalue_ref);
                upvalue_mut.close(value);

                // Remove from open list
                self.open_upvalues = next;
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
            Value::Class(class) => {
                // Create a new instance
                self.maybe_collect();
                let instance = crate::object::LoxInstance::new(class);
                let instance_ref = self.gc.alloc(instance)
                    .map_err(|_| self.runtime_error("Out of memory."))?;
                // Replace the class on the stack with the instance
                let stack_slot = self.stack.len() - arg_count as usize - 1;
                self.stack[stack_slot] = Value::Instance(instance_ref);
                Ok(())
            }
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    /// Call a Lox closure
    fn call(&mut self, closure: GcRef<LoxClosure>, arg_count: u8) -> Result<(), LoxError> {
        // Check arity
        let closure_obj = self.gc.deref(closure);
        let function = self.gc.deref(closure_obj.function);
        
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
        // The slot_offset points to the closure object on the stack
        let slot_offset = self.stack.len() - arg_count as usize - 1;
        let frame = CallFrame::new(closure, slot_offset);
        self.frames.push(frame);
        Ok(())
    }

    /// Call a native function
    fn call_native(&mut self, native: GcRef<NativeFunction>, arg_count: u8) -> Result<(), LoxError> {
        // Check arity
        let native_obj = self.gc.deref(native);
        if arg_count != native_obj.arity {
            return Err(self.runtime_error(&format!(
                "Expected {} arguments but got {}.",
                native_obj.arity, arg_count
            )));
        }

        let args_start = self.stack.len() - arg_count as usize;
        let result = {
            let args = &self.stack[args_start..];
            (native_obj.function)(args).map_err(|e| self.runtime_error(&e))?
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
        let chunk = frame.chunk(&self.gc);
        let line = *chunk.lines.get(ip).unwrap_or(&0);
        let span = chunk.span(ip).unwrap_or((0, 0));

        // Build stack trace
        let mut stack_trace = String::new();
        stack_trace.push_str("Stack trace:\n");
        for frame in self.frames.iter().rev() {
            let closure = self.gc.deref(frame.closure);
            let function = self.gc.deref(closure.function);
            let frame_line = function.chunk.lines.get(frame.ip.saturating_sub(1)).unwrap_or(&0);

            let name = match function.name {
                Some(n) => {
                    let name_str = self.gc.deref(n);
                    format!("{}()", name_str)
                }
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

    /// Convert a CompilerFunction (with Rc strings) to a runtime LoxFunction (with GcRef strings)
    /// This recursively converts all string constants and nested function constants in the chunk
    fn convert_function(cf: CompilerFunction, gc: &mut Gc) -> Result<LoxFunction, GcError> {
        // Convert the function name
        let name = match cf.name {
            Some(rc_name) => Some(gc.intern(&rc_name)?),
            None => None,
        };

        // Convert all constants in the chunk
        let mut new_constants = Vec::new();
        for constant in &cf.chunk.constants {
            let new_constant = match constant {
                Value::CompilerString(rc_str) => {
                    // Intern the string in the GC
                    let gc_str = gc.intern(rc_str.as_ref())?;
                    Value::String(gc_str)
                }
                Value::CompilerFunction(rc_func) => {
                    // Recursively convert nested function
                    // First clone the CompilerFunction out of the Rc
                    let nested_cf = CompilerFunction {
                        arity: rc_func.arity,
                        upvalue_count: rc_func.upvalue_count,
                        chunk: rc_func.chunk.clone(),
                        name: rc_func.name.clone(),
                    };
                    let nested_lf = Self::convert_function(nested_cf, gc)?;
                    // Allocate it in the GC
                    let gc_func = gc.alloc(nested_lf)?;
                    Value::Function(gc_func)
                }
                // Copy other value types as-is
                Value::Number(n) => Value::Number(*n),
                Value::Bool(b) => Value::Bool(*b),
                Value::Nil => Value::Nil,
                // Runtime variants shouldn't appear in compiler output
                Value::String(_) | Value::Function(_) | Value::Closure(_) | Value::NativeFunction(_) | Value::Class(_) | Value::Instance(_) => {
                    return Err(GcError);
                }
            };
            new_constants.push(new_constant);
        }

        let mut new_chunk = Chunk::new();
        new_chunk.code = cf.chunk.code;
        new_chunk.lines = cf.chunk.lines;
        new_chunk.spans = cf.chunk.spans;
        new_chunk.constants = new_constants;

        Ok(LoxFunction {
            arity: cf.arity,
            upvalue_count: cf.upvalue_count,
            chunk: new_chunk,
            name,
        })
    }

    /// Interprets a compiled function
    /// @param cf The compiled function to execute
    /// @return Result indicating if the function was successfully interpreted
    pub fn interpret(cf: CompilerFunction) -> LoxResult<Value> {
        let mut vm = Self::new(false);

        // Convert compiler function to runtime function
        let function = Self::convert_function(cf, &mut vm.gc)
            .map_err(|_| vec![LoxError::RuntimeError(
                RuntimeError::new("Out of memory during function conversion.", (0, 0), 0)
            )])?;

        // Allocate the function and wrap it in a closure
        let function_ref = vm.gc.alloc(function)
            .map_err(|_| vec![LoxError::RuntimeError(
                RuntimeError::new("Out of memory.", (0, 0), 0)
            )])?;
        let closure = LoxClosure::new(function_ref);
        let closure_ref = vm.gc.alloc(closure)
            .map_err(|_| vec![LoxError::RuntimeError(
                RuntimeError::new("Out of memory.", (0, 0), 0)
            )])?;

        // Push the closure onto the stack
        vm.push(Value::Closure(closure_ref));

        // Create initial call frame for the script closure
        let frame = CallFrame::new(closure_ref, 0);
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
    fn read_string(&self, idx: usize) -> Result<GcRef<String>, LoxError> {
        let frame = self.current_frame();
        let chunk = frame.chunk(&self.gc);
        match chunk.constant(idx) {
            Some(Value::String(s)) => Ok(*s),
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
            Value::String(_) => write!(f, "<string>"),
            Value::Function(_) => write!(f, "<fn>"),
            Value::Closure(_) => write!(f, "<closure>"),
            Value::NativeFunction(_) => write!(f, "<native fn>"),
            Value::Class(_) => write!(f, "<class>"),
            Value::Instance(_) => write!(f, "<instance>"),
            Value::CompilerString(s) => write!(f, "{}", s),
            Value::CompilerFunction(_) => write!(f, "<fn>"),
        }
    }
}
