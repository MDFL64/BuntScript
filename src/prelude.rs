use crate::program::Program;

static PRELUDE_SOURCE: &str = include_str!("prelude.bs");

pub fn load_prelude<'a,S>(program: &'a Program<'a,S>) {
    let prelude = program.load_raw("prelude", PRELUDE_SOURCE).unwrap();

    bunt_define!(
        prelude,
        fn assert_eq_num(x: f64, y: f64) {
            assert_eq!(x, y);
        }
    )
    .unwrap();

    bunt_define!(
        prelude,
        fn assert_eq_bool(x: bool, y: bool) {
            assert_eq!(x, y);
        }
    )
    .unwrap();

    bunt_define!(
        prelude,
        fn print_num(x: f64) {
            println!("{x}");
        }
    )
    .unwrap();

    bunt_define!(
        prelude,
        fn print_bool(x: bool) {
            println!("{x}");
        }
    )
    .unwrap();

    bunt_define!(
        prelude,
        fn exit(code: f64) {
            std::process::exit(code as i32);
        }
    )
    .unwrap();

    prelude.make_prelude();
}
