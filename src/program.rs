use std::cell::RefCell;
use std::{marker::PhantomData, path::Path};

use crate::errors::{CompileError, CompileErrorKind};
use crate::front::{FrontEnd, ModuleItems, Sig};
use crate::type_convert::{ArgValue, RetValue};

pub struct Program<'a, S> {
    front: FrontEnd<'a>,
    _state_ty: PhantomData<S>,
}

pub struct Module<'a, S> {
    items: &'a ModuleItems<'a>,
    program: &'a Program<'a,S>,
    _state_ty: PhantomData<S>,
}

pub trait WrapBuntFunc<S> {
    type Closure;

    fn bunt_sig<'a>(front: &'a FrontEnd<'a>) -> Sig<'a>;

    unsafe fn wrap(raw_ptr: *const u8) -> Self::Closure;
}

macro_rules! impl_wrapped_bunt {
    (( $($args_t:ident),* ), ( $($args_e:ident),* )) => {
        impl<S,R,$($args_t),*> WrapBuntFunc<S> for fn($($args_t),*)->R
            where R: RetValue + 'static, $($args_t : ArgValue + 'static),*
        {
            type Closure = Box<dyn Fn(S,$($args_t),*)->R>;

            fn bunt_sig<'a>(program: &'a FrontEnd<'a>) -> Sig<'a> {
                Sig{
                    args: vec!( $($args_t ::bunt_type(program)),* ),
                    result: R::bunt_type(program)
                }
            }

            unsafe fn wrap(raw_ptr: *const u8) -> Self::Closure {
                let raw_fn: unsafe extern "C" fn( $($args_t ::AbiType),* ) -> R::AbiType = std::mem::transmute(raw_ptr);

                Box::new(move |_state,$($args_e),*| {
                    let a: R::AbiType = raw_fn( $($args_e .to_bunt()),* );
                    R::from_bunt(a)
                })
            }
        }
    }
}

impl_wrapped_bunt!((), ());
impl_wrapped_bunt!((A), (a));
impl_wrapped_bunt!((A, B), (a, b));
impl_wrapped_bunt!((A, B, C), (a, b, c));
impl_wrapped_bunt!((A, B, C, D), (a, b, c, d));
impl_wrapped_bunt!((A, B, C, D, E), (a, b, c, d, e));
impl_wrapped_bunt!((A, B, C, D, E, F), (a, b, c, d, e, f));

impl<'a, S> Program<'a, S> {
    pub fn new(source_root: impl AsRef<Path>) -> Self {
        Self {
            front: FrontEnd::new(source_root.as_ref().to_owned()),
            _state_ty: PhantomData::default(),
        }
    }

    pub fn load_module(&'a self, path: impl AsRef<Path>) -> Result<Module<'a, S>, CompileError> {
        let items = self.front.module(path.as_ref()).items()?;

        Ok(Module {
            items,
            program: self,
            _state_ty: PhantomData::default(),
        })
    }

    /*let path = path.as_ref();
    let source = std::fs::read_to_string(path)
        .map_err(|_| Self::error(CompileErrorKind::FileReadFailed(path.to_owned())))?;

    Parser::parse_module(&source, path.to_owned());

    self.finalize()?;

    Ok(mod_id)*/
    //}

    /*fn finalize(&'vm self) -> Result<(), CompileError> {
        // todo finalize type constraints

        let modules = self.raw.modules.borrow();
        let mut compiler = self.compiler.borrow_mut();

        for (_, module) in modules.iter() {
            for (_, item) in module.iter() {
                if let ScopeValue::Function(func) = item {
                    func.clif_id.get_or_init(|| {
                        let full_name = format!("{}:{}", module.unique_name(), func.name);
                        compiler.declare(&full_name, &func.sig())
                    });
                }
            }
        }

        for (_, module) in modules.iter() {
            for (_, item) in module.iter() {
                if let ScopeValue::Function(func) = item {
                    compiler.compile(&self.raw, func);
                }
            }
        }

        compiler.finalize();

        Ok(())
    }*/
}

impl<'a,S> Module<'a,S> {
    pub fn get_function<F>(
        &'a self,
        name: &str,
    ) -> Result<F::Closure, CompileError>
    where
        F: WrapBuntFunc<S> + ?Sized,
    {
        let Some(func) = self.items.get(name) else {
            return Err(CompileError {
                kind: CompileErrorKind::CanNotResolve,
                message: format!("cannot resolve {:?}",name)
            });
        };

        if func.sig()?.ty_sig != F::bunt_sig(&self.program.front) {
            return Err(CompileError {
                kind: CompileErrorKind::TypeError,
                message: format!("signature mismatch")
            });
        }

        /*let func_id = func.clif_id.get().unwrap();
        let raw_ptr = self.compiler.borrow().get_code(*func_id);

        // safety: function signature has been checked
        unsafe { Ok(F::wrap(raw_ptr)) }*/
        panic!("failure");
    }
}
