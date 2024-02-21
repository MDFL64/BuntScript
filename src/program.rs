use std::path::Path;

use crate::{back::ProgramCompiler, checker::{resolve_ty, Checker}, front::{self, CompileError}, handle_vec::{Handle, HandleVec}, middle::{Module, Symbol}, types::Sig};

pub type ModuleHandle = Handle<Module>;

pub struct Program {
    modules: HandleVec<Module>,
    compiler: ProgramCompiler
}

pub enum GetFunctionError {
    FunctionNotFound,
    SignatureMismatch
}

impl Program {
    pub fn new() -> Self {
        Self {
            modules: HandleVec::default(),
            compiler: ProgramCompiler::new()
        }
    }

    pub fn load_module(&mut self, path: impl AsRef<Path>) -> Result<ModuleHandle, CompileError> {
        let new_mod = front::parse_module(path)?;
        let handle = self.modules.alloc(new_mod);

        // TODO parse submodules?

        self.declare_items()?;
        self.check()?;

        Ok(handle)
    }

    /// Do not call! Use the get_function! macro instead.
    pub fn get_function(&self, module: ModuleHandle, name: &str, sig: &Sig) -> Result<*mut u8,GetFunctionError> {
        let Some(func) = self.modules.get(module).items.get(name) else {
            return Err(GetFunctionError::FunctionNotFound)
        };

        if func.sig.get().unwrap() != sig {
            return Err(GetFunctionError::SignatureMismatch)
        }

        panic!("get function fr fr");
    }

    fn declare_items(&mut self) -> Result<(),CompileError> {

        for module in self.modules.iter() {
            for (func_name,func) in module.items.iter() {
                if func.sig.get().is_some() {
                    continue;
                }
    
                let full_name = format!("{}.{}",module.name.as_str(),func_name.as_str());

                let args = func.syn_args.iter().map(|(_,ty)| resolve_ty(*ty)).collect();
                let result = resolve_ty(func.syn_ret_ty);

                let sig = Sig{
                    args,
                    result
                };
                
                let func_id = self.compiler.declare(&full_name, &sig);
                
                func.clif_id.set(func_id).unwrap();
                func.sig.set(sig).unwrap();
            }
        }
        
        Ok(())
    }

    fn check(&mut self) -> Result<(),CompileError> {
        for module in self.modules.iter_mut() {
            Checker::check(module)?;
        }

        Ok(())
    }
}
