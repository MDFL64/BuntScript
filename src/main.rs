//mod back;
mod errors;
mod single_pass;

mod checker;
mod ir;
mod types;

mod handle_vec;

use std::{
    path::PathBuf,
    process::Output,
    time::{Duration, Instant},
};

use crate::{ir::RawProgram, single_pass::SinglePass};

fn main() {
    let mut program = RawProgram::new();

    let mod_id = SinglePass::compile(
        "function alpha(a: number,b: number,c: number): number {return a + b + c + 62}",
        PathBuf::from("meh.bs"),
        &mut program,
    )
    .unwrap();

    panic!("TODO {:?}", mod_id);

    /*let mut program = Program::<()>::new();
    let module = program.load_module("script/bingle.bs").unwrap();

    let add = program
        .get_function::<fn(f64,f64) -> f64>(module, "add")
        .unwrap();

    let start = Instant::now();
    let mut n = 0.0;
    for _ in 0..1_000_000_000 {
        n = add((),n,0.321);
    }
    println!("{:?} {}", start.elapsed(), n);*/
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
