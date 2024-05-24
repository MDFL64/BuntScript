mod program;

mod back;
mod front;

mod errors;
mod handle_vec;
mod type_convert;
mod util;

use program::Program;

fn main() {
    let mut pwd = std::env::current_dir().unwrap();
    pwd.push("script");

    let program = Program::<()>::new(&pwd);

    let module = program.load_module("simple.bs").unwrap();

    let func = module
        .get_function::<fn(f64, f64, f64) -> f64>("alpha")
        .unwrap();

    println!("good! {}", func((), 10.0, 10.0, 10.0));
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
