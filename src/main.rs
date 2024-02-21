mod front;

mod program;

mod middle;
mod types;
mod checker;

mod type_convert;
mod back;

mod handle_vec;

use program::Program;

// hack to make macros work
use crate as buntscript;

use bunt_macro::get_function;

// must be available to bunt-macro
pub use type_convert::ToBuntType;
pub use middle::Type;
pub use types::Sig;

fn main() {
    
    let mut program = Program::new();
    let mod_handle = program.load_module("test/bingle.bs").unwrap();

    let func: Option<_> = get_function!(program,mod_handle,"main",fn(f64, f64, f64) -> f64);
    let func = func.unwrap();

    func();

    /*let mut module = front::load_module("test/bingle.bs").expect("frontend error");

    Checker::fill_sigs(&module).unwrap();
    Checker::check(&mut module).unwrap();

    let module = CompiledModule::new(&module);*/

    /*Checker::check(&mut func).unwrap();
    
    let mut backend = Backend::new();
    let func_ptr = backend.compile(&func);
    
    unsafe {
        let func_native: unsafe extern "C" fn(f64, f64, f64) -> f64 =
        std::mem::transmute(func_ptr);

        let t = Instant::now();
        println!("{}", func_native(1_000_000_000.0, 5.0, 1.01));
        println!("bunt = {:?}",t.elapsed());

        let t = Instant::now();
        println!("{}", func_rust(1_000_000_000.0, 5.0, 1.01));
        println!("rust = {:?}",t.elapsed());

        shell_dump(func_ptr);
    }

    //let ptr: Option<func> = get_function!(module,"main",fn (f64, f64, f64) -> f64);*/

}

// very bad function for dumping machine code, use only for debugging
unsafe fn shell_dump(code: *const u8) {
    const LENGTH: usize = 128;
    let code = std::slice::from_raw_parts(code, LENGTH);
    for c in code {
        eprint!("{:02x}",c);
    }
    eprintln!();
}

fn func_rust(limit: f64, a: f64, b: f64) -> f64 {
    let mut i = 0.0;
    let mut sum = 0.0;

    while i < limit {
        i = i + 1.0;
        sum = (sum + a) / b;
    };
    return sum;
}
