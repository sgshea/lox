mod chunk;
mod compiler;
mod error;
mod object;
mod scanner;
mod vm;

use std::rc::Rc;

use miette::Result;

pub use crate::chunk::Value;
use crate::compiler::compile;
pub use crate::error::LoxError;
use crate::object::LoxFunction;
pub use crate::object::StringInterner;
use crate::vm::VirtualMachine;

/// Vector error type for the language
pub type LoxResult<T> = Result<T, Vec<LoxError>>;

/// Main way to run the language
/// @param source_code - The source code to interpret
/// @param source_name - The name of the source file
pub fn interpret(source_code: &str, source_name: &str) -> LoxResult<Value> {
    // Compile source into a function
    let function: Rc<LoxFunction> = compile(source_code, source_name)?;

    // Create vm and run
    VirtualMachine::interpret(function)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_arithmetic() {
        let result = interpret("print 1 + 2;", "test").unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_function_declaration() {
        let result = interpret(
            r#"
            fun greet() {
                print "Hello!";
            }
            greet();
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_function_with_return() {
        let result = interpret(
            r#"
            fun add(a, b) {
                return a + b;
            }
            print add(1, 2);
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_recursive_function() {
        let result = interpret(
            r#"
            fun fib(n) {
                if (n < 2) return n;
                return fib(n - 1) + fib(n - 2);
            }
            print fib(10);
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_native_clock() {
        let result = interpret(
            r#"
            var t = clock();
            print t;
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_function_as_value() {
        let result = interpret(
            r#"
            fun sayHi() {
                print "Hi!";
            }
            print sayHi;
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_nested_functions() {
        let result = interpret(
            r#"
            fun outer() {
                fun inner() {
                    return 42;
                }
                return inner();
            }
            print outer();
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }
}
