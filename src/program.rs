use std::cell::RefCell;
use std::path::PathBuf;
use std::{marker::PhantomData, path::Path};

use crate::back::ProgramCompiler;
use crate::errors::{CompileError, CompileErrorKind, CompileErrorSource};
use crate::ir::ModuleHandle;
use crate::single_pass::{ScopeValue, SinglePass};
use crate::type_convert::{ArgValue, RetValue};
use crate::{ir::RawProgram, types::Sig};

pub struct Program<'vm,S> {
    raw: RawProgram<'vm>,
    compiler: RefCell<ProgramCompiler>,
    _state_ty: PhantomData<S>,
}

pub trait WrapBuntFunc<S> {
    type Closure;

    fn bunt_sig<'vm>(program: &'vm RawProgram<'vm>) -> Sig<'vm>;

    unsafe fn wrap(raw_ptr: *const u8) -> Self::Closure;
}

macro_rules! impl_wrapped_bunt {
    (( $($args_t:ident),* ), ( $($args_e:ident),* )) => {
        impl<S,R,$($args_t),*> WrapBuntFunc<S> for fn($($args_t),*)->R
            where R: RetValue + 'static, $($args_t : ArgValue + 'static),*
        {
            type Closure = Box<dyn Fn(S,$($args_t),*)->R>;

            fn bunt_sig<'vm>(program: &'vm RawProgram<'vm>) -> Sig<'vm> {
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

impl_wrapped_bunt!((),              ());
impl_wrapped_bunt!((A),             (a));
impl_wrapped_bunt!((A,B),           (a,b));
impl_wrapped_bunt!((A,B,C),         (a,b,c));
impl_wrapped_bunt!((A,B,C,D),       (a,b,c,d));
impl_wrapped_bunt!((A,B,C,D,E),     (a,b,c,d,e));
impl_wrapped_bunt!((A,B,C,D,E,F),   (a,b,c,d,e,f));

impl<'vm,S> Program<'vm,S> {
    pub fn new() -> Self {
        Self {
            raw: RawProgram::new(),
            compiler: RefCell::new(ProgramCompiler::new()),
            _state_ty: PhantomData::default(),
        }
    }

    fn error(kind: CompileErrorKind) -> CompileError {
        CompileError{
            kind,
            source: CompileErrorSource::Rust
        }
    }

    pub fn load_module(&'vm self, path: impl AsRef<Path>) -> Result<ModuleHandle, CompileError> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(path).map_err(|_| {
            Self::error(CompileErrorKind::FileReadFailed(path.to_owned()))
        })?;

        let mod_id = SinglePass::compile(
            &source,
            path.to_owned(),
            &self.raw,
        )
        .unwrap();

        self.finalize()?;

        Ok(mod_id)
    }

    fn finalize(&'vm self) -> Result<(),CompileError> {
        // todo finalize type constraints

        let modules = self.raw.modules.borrow();
        let mut compiler = self.compiler.borrow_mut();
        
        for module in modules.iter() {
            for (_,item) in module.iter() {
                if let ScopeValue::Function(func) = item {
                    func.clif_id.get_or_init(|| {
                        let full_name = format!("{}:{}",module.unique_name(),func.name);
                        compiler.declare(&full_name, &func.sig())
                    });
                }
            }
        }

        for module in modules.iter() {
            for (_,item) in module.iter() {
                if let ScopeValue::Function(func) = item {
                    compiler.compile(&self.raw, func);
                }
            }
        }

        compiler.finalize();

        Ok(())
    }

    /// Do not call! Use the get_function! macro instead.
    pub fn get_function<F>(
        &'vm self,
        module: ModuleHandle,
        name: &str,
    ) -> Result<F::Closure, CompileError>
    where
        F: WrapBuntFunc<S> + ?Sized,
    {
        let modules = self.raw.modules.borrow();

        let Some(item) = modules.get(module).get(name) else {
            return Err(Self::error(CompileErrorKind::CanNotResolve(name.to_owned())));
        };

        let ScopeValue::Function(func) = item else {
            return Err(Self::error(CompileErrorKind::TypeError(format!("'{name}' is not a function"))));
        };

        if func.sig() != F::bunt_sig(&self.raw) {
            return Err(Self::error(CompileErrorKind::TypeError("signature mismatch".to_owned())));
        }

        let func_id = func.clif_id.get().unwrap();
        let raw_ptr = self.compiler.borrow().get_code(*func_id);

        // safety: function signature has been checked
        unsafe { Ok(F::wrap(raw_ptr)) }
    }
}
