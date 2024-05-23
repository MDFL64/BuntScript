mod program;

mod front;

mod errors;
mod handle_vec;
mod util;
mod type_convert;

use program::Program;

fn main() {
    let mut pwd = std::env::current_dir().unwrap();
    pwd.push("script");

    let program = Program::<()>::new(&pwd);

    let module = program.load_module("simple.bs").unwrap();

    let func = module.get_function::<fn(f64,f64,f64)->f64>("alpha").unwrap();

    println!("good :)");

    /*let mod_id = program.load_module("script/simple.bs").unwrap();

    let add = program
        .get_function::<fn(f64,f64,f64) -> f64>(mod_id, "alpha")
        .unwrap();

    let start = Instant::now();
    let mut n = 0.0;
    for _ in 0..1_000_000_000 {
        n = add((),n,0.321,0.5);
    }
    println!("{:?} {}", start.elapsed(), n);*/

    /*let source_path = PathBuf::from("script/simple.bs");

    let source = std::fs::read_to_string(&source_path).unwrap();
    let mut file = SourceFile::new(source_path, source).unwrap();

    let res = file.test_func("alpha");
    println!(">>> {:?}",res);*/
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
