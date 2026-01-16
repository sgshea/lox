use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use crate::chunk::{Chunk, Value};

/// Represents a compiled Lox function
pub struct LoxFunction {
    /// Number of parameters the function expects
    pub arity: u8,
    /// Number of upvalues the function captures
    pub upvalue_count: usize,
    /// The function's bytecode
    pub chunk: Chunk,
    /// Function name (None for top-level script)
    pub name: Option<Rc<String>>,
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
    pub fn with_name(name: Rc<String>) -> Self {
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
        match &self.name {
            Some(name) => write!(f, "<fn {}>", name),
            None => write!(f, "<script>"),
        }
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(f, "<fn {}>", name),
            None => write!(f, "<script>"),
        }
    }
}

/// Represents a closure which is a function plus its captured variables
pub struct LoxClosure {
    /// The underlying function
    pub function: Rc<LoxFunction>,
    /// Captured upvalues
    pub upvalues: Vec<Rc<RefCell<LoxUpvalue>>>,
}

impl LoxClosure {
    /// Creates a new closure wrapping a function
    pub fn new(function: Rc<LoxFunction>) -> Self {
        let upvalue_count = function.upvalue_count;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
        }
    }
}

impl fmt::Display for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function)
    }
}

impl fmt::Debug for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.function)
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
    pub next: Option<Rc<RefCell<LoxUpvalue>>>,
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
    strings: HashSet<Rc<String>>,
}

impl StringInterner {
    /// Creates a new empty string interner
    pub fn new() -> Self {
        Self {
            strings: HashSet::new(),
        }
    }

    /// Interns a string, returning an Rc to either an existing interned string
    /// or a newly created one if this string hasn't been seen before
    pub fn intern(&mut self, s: &str) -> Rc<String> {
        // Try to find an existing interned string
        for existing in self.strings.iter() {
            if existing.as_str() == s {
                return Rc::clone(existing);
            }
        }

        // Create new interned string
        let rc = Rc::new(s.to_string());
        self.strings.insert(Rc::clone(&rc));
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_same_string_returns_same_rc() {
        let mut interner = StringInterner::new();
        let s1 = interner.intern("hello");
        let s2 = interner.intern("hello");

        // Should point to the same allocation
        assert!(Rc::ptr_eq(&s1, &s2));
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

    #[test]
    fn test_lox_function_display() {
        let func = LoxFunction::new();
        assert_eq!(format!("{}", func), "<script>");

        let mut interner = StringInterner::new();
        let name = interner.intern("myFunc");
        let named_func = LoxFunction::with_name(name);
        assert_eq!(format!("{}", named_func), "<fn myFunc>");
    }

    #[test]
    fn test_native_function_display() {
        fn dummy(_args: &[Value]) -> Result<Value, String> {
            Ok(Value::Nil)
        }
        let native = NativeFunction::new("clock", 0, dummy);
        assert_eq!(format!("{}", native), "<native fn>");
        assert_eq!(format!("{:?}", native), "<native fn clock>");
    }

    #[test]
    fn test_lox_closure() {
        let func = Rc::new(LoxFunction::new());
        let closure = LoxClosure::new(func);
        assert_eq!(format!("{}", closure), "<script>");
        assert!(closure.upvalues.is_empty());
    }

    #[test]
    fn test_lox_upvalue_open() {
        let upvalue = LoxUpvalue::new(5);
        assert!(upvalue.is_open());
        assert_eq!(upvalue.location, Some(5));
    }

    #[test]
    fn test_lox_upvalue_close() {
        let mut upvalue = LoxUpvalue::new(5);
        upvalue.close(Value::Number(42.0));
        assert!(!upvalue.is_open());
        assert_eq!(upvalue.location, None);
        assert_eq!(upvalue.closed, Value::Number(42.0));
    }
}
