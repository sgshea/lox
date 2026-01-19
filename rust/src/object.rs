use std::any::Any;
use std::fmt;
use std::rc::Rc;

use crate::chunk::{Chunk, Value};
use crate::gc::{Gc, GcRef, GcTrace};

/// Compiler-time representation of a Lox function (uses Rc for strings)
#[derive(Debug)]
pub struct CompilerFunction {
    /// Number of parameters the function expects
    pub arity: u8,
    /// Number of upvalues the function captures
    pub upvalue_count: usize,
    /// The function's bytecode
    pub chunk: Chunk,
    /// Function name (None for top-level script)
    pub name: Option<Rc<String>>,
}

impl CompilerFunction {
    pub fn new() -> Self {
        Self {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }
}

impl Default for CompilerFunction {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents a compiled Lox function (runtime version with GC references)
pub struct LoxFunction {
    /// Number of parameters the function expects
    pub arity: u8,
    /// Number of upvalues the function captures
    pub upvalue_count: usize,
    /// The function's bytecode
    pub chunk: Chunk,
    /// Function name (None for top-level script)
    pub name: Option<GcRef<String>>,
}

impl LoxFunction {
    /// Creates a new empty function
    pub fn new() -> Self {
        Self {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: None,
        }
    }

    /// Creates a new function with the given name
    pub fn with_name(name: GcRef<String>) -> Self {
        Self {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: Some(name),
        }
    }
}

impl Default for LoxFunction {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn>")
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn>")
    }
}

/// Represents a closure which is a function plus its captured variables
pub struct LoxClosure {
    /// The underlying function
    pub function: GcRef<LoxFunction>,
    /// Captured upvalues
    pub upvalues: Vec<GcRef<LoxUpvalue>>,
}

impl LoxClosure {
    /// Creates a new closure wrapping a function
    pub fn new(function: GcRef<LoxFunction>) -> Self {
        Self {
            function,
            upvalues: Vec::new(),
        }
    }
}

impl fmt::Display for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

impl fmt::Debug for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

/// Represents a captured variable (upvalue)
pub struct LoxUpvalue {
    /// The captured value's location
    /// - Open: Some(index) into VM stack
    /// - Closed: None (value stored in `closed`)
    pub location: Option<usize>,

    /// Storage for closed-over value (when off stack)
    pub closed: Value,

    /// Link to next open upvalue (for VM's open upvalue list)
    /// Only used when location.is_some()
    pub next: Option<GcRef<LoxUpvalue>>,
}

impl LoxUpvalue {
    /// Creates a new open upvalue pointing to a stack slot
    pub fn new(stack_index: usize) -> Self {
        Self {
            location: Some(stack_index),
            closed: Value::Nil,
            next: None,
        }
    }

    /// Returns true if this upvalue is still open (on stack)
    pub fn is_open(&self) -> bool {
        self.location.is_some()
    }

    /// Closes this upvalue, moving the value from stack to heap
    pub fn close(&mut self, value: Value) {
        self.closed = value;
        self.location = None;
    }
}

impl fmt::Debug for LoxUpvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(loc) = self.location {
            write!(f, "<upvalue open@{}>", loc)
        } else {
            write!(f, "<upvalue closed={:?}>", self.closed)
        }
    }
}

/// Describes how to capture an upvalue (used in bytecode)
#[derive(Debug, Clone, Copy)]
pub struct UpvalueDescriptor {
    /// If true, captures a local from the enclosing function
    /// If false, captures an upvalue from the enclosing function
    pub is_local: bool,
    /// Index of the local slot or upvalue to capture
    pub index: u8,
}

/// Represents a method bound to an instance
pub struct LoxBoundMethod {
    /// The instance (receiver) this method is bound to
    pub receiver: Value,
    /// The method closure
    pub method: GcRef<LoxClosure>,
}

impl LoxBoundMethod {
    pub fn new(receiver: Value, method: GcRef<LoxClosure>) -> Self {
        Self { receiver, method }
    }
}

impl fmt::Debug for LoxBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound method>")
    }
}

impl fmt::Display for LoxBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<bound method>")
    }
}

/// Type alias for native function implementations
/// Takes a slice of argument values and returns a Result
pub type NativeFn = fn(args: &[Value]) -> Result<Value, String>;

/// Represents a native (built-in) function
pub struct NativeFunction {
    /// The name of the native function
    pub name: &'static str,
    /// Number of parameters the function expects
    pub arity: u8,
    /// The native function implementation
    pub function: NativeFn,
}

impl NativeFunction {
    /// Creates a new native function
    pub fn new(name: &'static str, arity: u8, function: NativeFn) -> Self {
        Self {
            name,
            arity,
            function,
        }
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}

impl fmt::Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

/// String interner for deduplicating strings
pub struct StringInterner {
    strings: std::collections::HashSet<std::rc::Rc<String>>,
}

/// Represents a Lox class
pub struct LoxClass {
    /// The class name
    pub name: GcRef<String>,
    /// Methods of the class (method name -> closure)
    pub methods: std::collections::HashMap<GcRef<String>, Value>,
}

impl LoxClass {
    pub fn new(name: GcRef<String>) -> Self {
        Self { 
            name,
            methods: std::collections::HashMap::new(),
        }
    }
}

impl fmt::Display for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class>")
    }
}

impl fmt::Debug for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class>")
    }
}

/// Represents an instance of a Lox class
pub struct LoxInstance {
    /// The class this is an instance of
    pub klass: GcRef<LoxClass>,
    /// Instance fields (property name -> value)
    pub fields: std::collections::HashMap<GcRef<String>, Value>,
}

impl LoxInstance {
    pub fn new(klass: GcRef<LoxClass>) -> Self {
        Self {
            klass,
            fields: std::collections::HashMap::new(),
        }
    }
}

impl fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<instance>")
    }
}

impl fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<instance>")
    }
}

impl StringInterner {
    /// Creates a new empty string interner
    pub fn new() -> Self {
        Self {
            strings: std::collections::HashSet::new(),
        }
    }

    /// Interns a string, returning an Rc to either an existing interned string
    /// or a newly created one if this string hasn't been seen before
    pub fn intern(&mut self, s: &str) -> std::rc::Rc<String> {
        // Try to find an existing interned string
        for existing in self.strings.iter() {
            if existing.as_str() == s {
                return std::rc::Rc::clone(existing);
            }
        }

        // Create new interned string
        let rc = std::rc::Rc::new(s.to_string());
        self.strings.insert(std::rc::Rc::clone(&rc));
        rc
    }

    /// Returns the number of unique interned strings
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Returns true if the interner is empty
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

// GcTrace implementations for all GC-managed types

impl GcTrace for String {
    fn trace(&self, _gc: &mut Gc) {
        // Strings contain no GC references
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl GcTrace for LoxFunction {
    fn trace(&self, gc: &mut Gc) {
        // Mark the function name if present
        if let Some(name) = self.name {
            gc.mark_object(name);
        }
        
        // Mark all constants in the chunk
        for constant in &self.chunk.constants {
            gc.mark_value(constant);
        }
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl GcTrace for LoxClosure {
    fn trace(&self, gc: &mut Gc) {
        // Mark the underlying function
        gc.mark_object(self.function);
        
        // Mark all captured upvalues
        for &upvalue in &self.upvalues {
            gc.mark_object(upvalue);
        }
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl GcTrace for LoxUpvalue {
    fn trace(&self, gc: &mut Gc) {
        // Mark the closed value
        gc.mark_value(&self.closed);
        
        // Mark the next upvalue in the chain
        if let Some(next) = self.next {
            gc.mark_object(next);
        }
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl GcTrace for NativeFunction {
    fn trace(&self, _gc: &mut Gc) {
        // Native functions contain no GC references
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl GcTrace for LoxClass {
    fn trace(&self, gc: &mut Gc) {
        // Mark the class name
        gc.mark_object(self.name);
        // Mark all method keys and values
        for (&key, value) in &self.methods {
            gc.mark_object(key);
            gc.mark_value(value);
        }
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl GcTrace for LoxBoundMethod {
    fn trace(&self, gc: &mut Gc) {
        // Mark the receiver
        gc.mark_value(&self.receiver);
        // Mark the method closure
        gc.mark_object(self.method);
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl GcTrace for LoxInstance {
    fn trace(&self, gc: &mut Gc) {
        // Mark the class
        gc.mark_object(self.klass);
        // Mark all field keys and values
        for (&key, value) in &self.fields {
            gc.mark_object(key);
            gc.mark_value(value);
        }
    }
    
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_same_string_returns_same_rc() {
        let mut interner = StringInterner::new();
        let s1 = interner.intern("hello");
        let s2 = interner.intern("hello");

        // Should point to the same allocation
        assert!(std::rc::Rc::ptr_eq(&s1, &s2));
    }

    #[test]
    fn test_intern_different_strings() {
        let mut interner = StringInterner::new();
        let s1 = interner.intern("hello");
        let s2 = interner.intern("world");

        // Should be different strings
        assert_ne!(s1, s2);
    }

    #[test]
    fn test_intern_count() {
        let mut interner = StringInterner::new();
        interner.intern("hello");
        interner.intern("world");
        interner.intern("hello"); // Duplicate

        assert_eq!(interner.len(), 2);
    }

    // Note: Tests for GC-managed objects require a Gc instance
    // These tests have been temporarily disabled during GC implementation
}
