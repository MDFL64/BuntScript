#[macro_use]
mod program;

mod back;
mod front;

mod errors;
mod handle_vec;
mod type_convert;
mod util;

use std::{fs, path::PathBuf, time::Instant};

use program::Program;

fn main() {
    let Some(file_path) = std::env::args().skip(1).next() else {
        fail("no script file provided");
    };
    let Ok(file_path) = fs::canonicalize(file_path) else {
        fail("can't find script file");
    };

    let mut source_root = file_path;
    let file_name = PathBuf::from(source_root.file_name().unwrap());
    assert!(source_root.pop());

    let program = Program::<()>::new(&source_root);
    let module = program.load_module(file_name).unwrap();

    bunt_define!(
        module,
        fn assert_eq_n(x: f64, y: f64) {
            assert_eq!(x, y);
        }
    )
    .unwrap();

    let func = bunt_use!(module, fn main() -> ()).unwrap();
    func(());
}

fn fail(msg: &str) -> ! {
    eprintln!("failure: {}", msg);
    std::process::exit(1);
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
