mod chunk;
mod compiler;
mod error;
mod gc;
mod object;
mod scanner;
mod vm;

use miette::Result;

pub use crate::chunk::Value;
use crate::compiler::compile;
pub use crate::error::LoxError;
pub use crate::object::{StringInterner, CompilerFunction};
use crate::vm::VirtualMachine;

/// Vector error type for the language
pub type LoxResult<T> = Result<T, Vec<LoxError>>;

/// Main way to run the language
/// @param source_code - The source code to interpret
/// @param source_name - The name of the source file
pub fn interpret(source_code: &str, source_name: &str) -> LoxResult<Value> {
    // Compile source into a function
    let function = compile(source_code, source_name)?;

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

    #[test]
    fn test_basic_closure() {
        let result = interpret(
            r#"
            fun outer() {
                var x = "outside";
                fun inner() {
                    print x;
                }
                inner();
            }
            outer();
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_closure_returns_inner_function() {
        let result = interpret(
            r#"
            fun outer() {
                var x = "outside";
                fun inner() {
                    print x;
                }
                return inner;
            }
            var fn = outer();
            fn();
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_closure_captures_variable() {
        let result = interpret(
            r#"
            fun makeCounter() {
                var count = 0;
                fun increment() {
                    count = count + 1;
                    print count;
                }
                return increment;
            }
            var counter = makeCounter();
            counter();
            counter();
            counter();
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_closure_shared_capture() {
        let result = interpret(
            r#"
            fun makePair() {
                var value = 0;
                fun getValue() {
                    return value;
                }
                fun setValue(n) {
                    value = n;
                }
                print getValue();
                setValue(42);
                print getValue();
            }
            makePair();
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }

    #[test]
    fn test_nested_closures() {
        let result = interpret(
            r#"
            fun outer() {
                var x = "outer";
                fun middle() {
                    fun inner() {
                        print x;
                    }
                    inner();
                }
                middle();
            }
            outer();
            "#,
            "test",
        )
        .unwrap();
        assert_eq!(result, Value::Nil);
    }
}
