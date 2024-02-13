mod front;

mod middle;
mod types;
mod checker;

mod back;

mod handle_vec;

use back::Backend;
use checker::Checker;

//use lalrpop_util::lalrpop_mod;

fn main() {
    let mut func = front::load_script("test/bingle.bs").expect("frontend error");
    Checker::check(&mut func).unwrap();

    let mut backend = Backend::new();
    let func_ptr = backend.compile(&func);

    unsafe {
        let func_native: unsafe extern "C" fn(f64, f64, bool) -> f64 =
            std::mem::transmute(func_ptr);
        println!("{}", func_native(5.0, 5.0, true));
        println!("{}", func_native(5.0, 5.0, false));
    }
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
