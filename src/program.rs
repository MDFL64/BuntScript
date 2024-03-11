use std::{marker::PhantomData, path::Path};

use crate::{
    back::ProgramCompiler,
    checker::{resolve_ty, Checker},
    front::{self, CompileError},
    handle_vec::{Handle, HandleVec},
    middle::Module,
    type_convert::{ArgValue, RetValue},
    types::Sig,
};

pub type ModuleHandle = Handle<Module>;

pub struct Program<S> {
    modules: HandleVec<Module>,
    compiler: ProgramCompiler,
    _state_ty: PhantomData<S>,
}

pub trait WrapBuntFunc<S> {
    type Closure;

    fn bunt_sig() -> Sig;

    unsafe fn wrap(raw_ptr: *const u8) -> Self::Closure;
}

macro_rules! impl_wrapped_bunt {
    (( $($args_t:ident),* ), ( $($args_e:ident),* )) => {
        impl<S,R,$($args_t),*> WrapBuntFunc<S> for fn($($args_t),*)->R
            where R: RetValue + 'static, $($args_t : ArgValue + 'static),*
        {
            type Closure = Box<dyn Fn(S,$($args_t),*)->R>;

            fn bunt_sig() -> Sig {
                Sig{
                    args: vec!( $($args_t ::bunt_type()),* ),
                    result: R::bunt_type()
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

impl_wrapped_bunt!((),              ());
impl_wrapped_bunt!((A),             (a));
impl_wrapped_bunt!((A,B),           (a,b));
impl_wrapped_bunt!((A,B,C),         (a,b,c));
impl_wrapped_bunt!((A,B,C,D),       (a,b,c,d));
impl_wrapped_bunt!((A,B,C,D,E),     (a,b,c,d,e));
impl_wrapped_bunt!((A,B,C,D,E,F),   (a,b,c,d,e,f));

impl<S> Program<S> {
    pub fn new() -> Self {
        Self {
            modules: HandleVec::default(),
            compiler: ProgramCompiler::new(),
            _state_ty: PhantomData::default(),
        }
    }

    pub fn load_module(&mut self, path: impl AsRef<Path>) -> Result<ModuleHandle, CompileError> {
        let handle = self.load_internal(path)?;

        self.declare_items()?;
        self.check()?;
        self.compile()?;

        Ok(handle)
    }

    fn load_internal(&mut self, path: impl AsRef<Path>) -> Result<ModuleHandle, CompileError> {
        let new_mod = front::parse_module(path)?;
        let handle = self.modules.alloc(new_mod);

        // TODO recursively load sub-modules?

        Ok(handle)
    }

    /// Do not call! Use the get_function! macro instead.
    pub fn get_function<F>(
        &self,
        module: ModuleHandle,
        name: &str,
    ) -> Result<F::Closure, CompileError>
    where
        F: WrapBuntFunc<S> + ?Sized,
    {
        let Some(func) = self.modules.get(module).items.get(name) else {
            return Err(CompileError::ResolutionFailure);
        };

        if func.sig.get().unwrap() != &F::bunt_sig() {
            return Err(CompileError::TypeError);
        }

        let func_id = func.clif_id.get().unwrap();
        let raw_ptr = self.compiler.get_code(*func_id);

        // safety: function signature has been checked
        unsafe { Ok(F::wrap(raw_ptr)) }
    }

    fn declare_items(&mut self) -> Result<(), CompileError> {
        for module in self.modules.iter() {
            for (func_name, func) in module.items.iter() {
                if func.sig.get().is_some() {
                    continue;
                }

                let full_name = format!("{}.{}", module.name.as_str(), func_name.as_str());

                let args = func
                    .syn_args
                    .iter()
                    .map(|(_, ty)| resolve_ty(*ty))
                    .collect();
                let result = resolve_ty(func.syn_ret_ty);

                let sig = Sig { args, result };

                let func_id = self.compiler.declare(&full_name, &sig);

                func.clif_id.set(func_id).unwrap();
                func.sig.set(sig).unwrap();
            }
        }

        Ok(())
    }

    fn check(&mut self) -> Result<(), CompileError> {
        for module in self.modules.iter_mut() {
            Checker::check(module)?;
        }

        Ok(())
    }

    fn compile(&mut self) -> Result<(), CompileError> {
        for module in self.modules.iter_mut() {
            for (_, func) in module.items.iter() {
                self.compiler.compile(func);
            }
        }
        self.compiler.finalize();

        Ok(())
    }
}
