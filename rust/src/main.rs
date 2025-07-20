use lox_rust_vm::{LoxResult, Value, interpret};

fn file(path: &str) -> LoxResult<Value> {
    let source_code = std::fs::read_to_string(path).expect("Could not read file.");
    interpret(&source_code, path)
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    match args.len() {
        2 => {
            let result = file(&args[1]);
            match result {
                Ok(value) => println!("{:?}", value),
                Err(err) => {
                    // Print all errors
                    for error in err {
                        println!("{:?}", error);
                    }
                }
            }
        }
        _ => println!("Invalid arguments"),
    }
}
