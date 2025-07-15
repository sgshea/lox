use lox_rust_vm::VirtualMachine;

fn repl() {}

fn file(path: &str) {}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    match args.len() {
        1 => repl(),
        2 => file(&args[1]),
        _ => println!("Invalid arguments"),
    }
}
