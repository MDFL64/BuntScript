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
