use std::io::{self, Write};

use lox_rust_vm::{LoxResult, Value, interpret};

fn file(path: &str) -> LoxResult<Value> {
    let source_code = std::fs::read_to_string(path).expect("Could not read file.");
    interpret(&source_code, path)
}

fn repl() {
    let stdin = io::stdin();
    let mut line = String::new();

    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");
        line.clear();

        if stdin.read_line(&mut line).expect("Failed to read line") == 0 {
            break;
        }

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        match interpret(trimmed, "repl") {
            Ok(value) => println!("{}", value),
            Err(errors) => {
                for error in errors {
                    eprintln!("{:?}", error);
                }
            }
        }
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    match args.len() {
        1 => repl(),
        2 => match file(&args[1]) {
            Ok(value) => println!("{}", value),
            Err(errors) => {
                for error in errors {
                    eprintln!("{:}", error);
                }
            }
        },
        _ => {
            eprintln!("Usage: cargo run [path]");
            std::process::exit(64);
        }
    }
}
