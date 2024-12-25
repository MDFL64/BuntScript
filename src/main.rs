#[macro_use]
mod program;

mod back;
mod front;

mod errors;
mod handle_vec;
mod type_convert;
mod util;

use std::time::Instant;

use program::Program;

fn main() {
    let mut source_root = std::env::current_dir().unwrap();
    source_root.push("script");

    let program = Program::<()>::new(&source_root);
    let module = program.load_module("calls.bs").unwrap();

    bunt_define!(module, fn print_number(x: f64) {
        println!("{}",x);
    }).unwrap();

    let func = bunt_use!(module, fn test(x: f64) -> f64).unwrap();

    let n = func((), 123.456);

    println!("result: {}", n);
    
}

// very bad function for dumping machine code, use only for debugging
unsafe fn shell_dump(code: *const u8) {
    const LENGTH: usize = 128;
    let code = std::slice::from_raw_parts(code, LENGTH);
    for c in code {
        eprint!("{:02x}", c);
    }
    eprintln!();
}
