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
    let mut pwd = std::env::current_dir().unwrap();
    pwd.push("script");

    let program = Program::<()>::new(&pwd);

    {
        let module = program.load_module("simple.bs").unwrap();

        for func_name in ["alpha", "beta", "delta", "gamma", "omega"] {
            let start = Instant::now();
            let func = module
                .get_function::<fn(f64, f64, bool) -> f64>(func_name)
                .unwrap();

            let n = func((), 5.0, 20.0, true);
            let elapsed = start.elapsed();
            println!("{}: {} ({:?})", func_name, n, elapsed);

            let start = Instant::now();
            let n = func((), 5.0, 20.0, false);
            let elapsed = start.elapsed();
            println!("{}: {} ({:?})", func_name, n, elapsed);
        }
    }

    {
        let start = Instant::now();
        let module = program.load_module("bingle.bs").unwrap();

        let func = module
            .get_function::<fn(f64, f64, f64) -> f64>("test")
            .unwrap();
        let n = func((), 200_000_000.0, 1.0, 20.0);
        let elapsed = start.elapsed();
        println!("loop: {} ({:?})", n, elapsed);
    }

    {
        let start = Instant::now();
        let module = program.load_module("fibo.bs").unwrap();

        let func = module.get_function::<fn(f64) -> f64>("fibonacci").unwrap();
    }

    /*{
        let module = program.load_module("bingle.bs").unwrap();
    }*/
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
