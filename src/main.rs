mod front;

mod program;

mod checker;
mod middle;
mod types;

mod back;
mod type_convert;

mod handle_vec;

use std::{process::Output, time::{Duration, Instant}};

// must be available to buntscript-macro
pub use front::CompileError;
pub use middle::Type;
pub use program::{ModuleHandle, Program};
pub use types::Sig;

fn main() {
    let mut program = Program::<()>::new();
    let module = program.load_module("test/bingle.bs").unwrap();

    let get_n = program.get_function::<fn()->f64>(module, "get_n").unwrap();
    
    let start = Instant::now();
    for _ in 0..1_000_000_000 {
        get_n(());
    }
    println!("{:?}",start.elapsed());

    //let r = module.test(&mut (), 1_000_000_000.0, 100.0, 1.01);
    //println!("{}", r);
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

/*mod macro_test {
    // hack to make macros work
    use crate as buntscript;
    use buntscript_macro::bunt_interface;

    #[bunt_interface]
    impl ModScript {
        fn test(a: f64, b: f64, c: f64) -> f64;

        fn add(a: f64, b: f64) -> f64;
    }
}*/
