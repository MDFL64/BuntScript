mod program;
mod type_convert;

mod errors;
mod single_pass;

mod checker;
mod ir;
mod types;

mod back;

mod handle_vec;

use std::{
    path::PathBuf,
    process::Output,
    time::{Duration, Instant},
};

use crate::{ir::RawProgram, program::Program, single_pass::SinglePass};

fn main() {
    let program = Program::<()>::new();

    let mod_id = program.load_module("script/simple.bs").unwrap();

    let add = program
        .get_function::<fn(f64,f64,f64) -> f64>(mod_id, "alpha")
        .unwrap();

    let start = Instant::now();
    let mut n = 0.0;
    for _ in 0..1_000_000_000 {
        n = add((),n,0.321,0.5);
    }
    println!("{:?} {}", start.elapsed(), n);
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
