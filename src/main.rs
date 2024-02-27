mod front;

mod program;

mod checker;
mod middle;
mod types;

mod back;
mod type_convert;

mod handle_vec;

// must be available to buntscript-macro
pub use front::CompileError;
pub use middle::Type;
pub use program::{ModuleHandle, ModuleInterface, Program};
pub use type_convert::{FromBuntValue, ToBuntValue};
pub use types::Sig;

fn main() {
    let mut program = Program::new();
    let module: macro_test::ModScript<()> = program.load_module("test/bingle.bs").unwrap();

    let r = module.test(&mut (), 1_000_000_000.0, 100.0, 1.01);
    println!("{}", r);
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

fn func_rust(limit: f64, a: f64, b: f64) -> f64 {
    let mut i = 0.0;
    let mut sum = 0.0;

    while i < limit {
        i = i + 1.0;
        sum = (sum + a) / b;
    }
    return sum;
}

mod macro_test {
    // hack to make macros work
    use crate as buntscript;
    use buntscript_macro::bunt_interface;

    #[bunt_interface]
    impl ModScript {
        fn test(a: f64, b: f64, c: f64) -> f64;
    }
}
