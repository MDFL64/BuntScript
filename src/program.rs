use std::{marker::PhantomData, path::Path};

use crate::{
    back::ProgramCompiler, checker::{resolve_ty, Checker}, front::{self, CompileError}, handle_vec::{Handle, HandleVec}, middle::Module, type_convert::RetValue, types::Sig
};

pub type ModuleHandle = Handle<Module>;

pub struct Program<S> {
    modules: HandleVec<Module>,
    compiler: ProgramCompiler,
    _state_ty: PhantomData<S>,
}

pub trait WrappedBuntFunc<S> {
    type Closure;

    fn bunt_sig() -> Sig;

    unsafe fn wrap(raw_ptr: *const u8) -> Self::Closure;
}

impl<S,R> WrappedBuntFunc<S> for fn()->R where R: RetValue + 'static {
    type Closure = Box<dyn Fn(S)->R>;

    fn bunt_sig() -> Sig {
        Sig{
            args: vec!(),
            result: R::bunt_type()
        }
    }

    unsafe fn wrap(raw_ptr: *const u8) -> Self::Closure {
        let raw_fn: unsafe extern "C" fn() -> R::AbiType = std::mem::transmute(raw_ptr);

        Box::new(move |_state| {
            let a: R::AbiType = raw_fn();
            R::from_bunt(a)
        })
    }
}

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
        name: &str
    ) -> Result<F::Closure, CompileError> where F: WrappedBuntFunc<S> + ?Sized {
        let Some(func) = self.modules.get(module).items.get(name) else {
            return Err(CompileError::ResolutionFailure);
        };

        if func.sig.get().unwrap() != &F::bunt_sig() {
            return Err(CompileError::TypeError);
        }

        let func_id = func.clif_id.get().unwrap();
        let raw_ptr = self.compiler.get_code(*func_id);

        unsafe {
            Ok(F::wrap(raw_ptr))
        }

        /*Ok(self.compiler.get_code(*func_id))
        panic!();*/
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
