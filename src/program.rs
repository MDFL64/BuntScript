use std::cell::RefCell;
use std::{marker::PhantomData, path::Path};

use crate::back::BackEnd;
use crate::errors::{CompileError, CompileErrorKind};
use crate::front::{FrontEnd, ModuleItems, Sig, Type};
use crate::type_convert::{ArgValue, RetValue};

pub struct Program<'a, S> {
    front: FrontEnd<'a>,
    back: RefCell<BackEnd>,
    _state_ty: PhantomData<S>,
}

pub struct Module<'a, S> {
    program: &'a Program<'a, S>,
    items: &'a ModuleItems<'a>,
    _state_ty: PhantomData<S>,
}

impl<'a, S> Program<'a, S> {
    pub fn new(source_root: impl AsRef<Path>) -> Self {
        Self {
            front: FrontEnd::new(source_root.as_ref().to_owned()),
            back: RefCell::new(BackEnd::new()),
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
}

impl<'a, S> Module<'a, S> {
    pub fn compile_function(&self, name: &str, required_sig: Sig) -> Result<*const u8, CompileError> {
        if let Some(func) = self.items.get(name) {
            let func_sig = &func.sig()?.ty_sig;
            if func_sig != &required_sig {
                return Err(CompileError {
                    kind: CompileErrorKind::TypeError,
                    message: format!("signature mismatch: {:?} != {:?}", func_sig, required_sig),
                });
            }

            let mut back = self.program.back.borrow_mut();
            back.get_code(func)
        } else {
            Err(CompileError {
                kind: CompileErrorKind::CanNotResolve,
                message: format!("cannot resolve '{}'", name),
            })
        }
    }

    pub fn define_function(&self, name: &str, required_sig: Sig, func_ptr: *const u8) -> Result<(), CompileError> {
        if let Some(func) = self.items.get(name) {
            if !func.is_extern {
                return Err(CompileError{
                    kind: CompileErrorKind::CanNotResolve,
                    message: format!("cannot define function '{}', not extern",name)
                });
            }

            let func_sig = &func.sig()?.ty_sig;
            if func_sig != &required_sig {
                return Err(CompileError {
                    kind: CompileErrorKind::TypeError,
                    message: format!("signature mismatch: {:?} != {:?}", func_sig, required_sig),
                });
            }

            let mut back = self.program.back.borrow_mut();
            back.define_extern(func, func_ptr)
        } else {
            Err(CompileError {
                kind: CompileErrorKind::CanNotResolve,
                message: format!("cannot resolve '{}'", name),
            })
        }
    }

    pub fn get_arg_ty<T: ArgValue>(&self) -> Type {
        T::bunt_type(&self.program.front)
    }
    
    pub fn get_ret_ty<T: RetValue>(&self) -> Type {
        T::bunt_type(&self.program.front)
    }
}

macro_rules! bunt_use {
    ($module:expr, fn $name:ident( $( $args_e:ident: $args_t:ty ),* ) -> $ret_t:ty) => {
        {
            use $crate::type_convert::ConvertValue;
            use $crate::front::Sig;
    
            type RetTy = <$ret_t as ConvertValue>::AbiType;
            type RawFn = unsafe extern "C" fn( $(<$args_t as ConvertValue>::AbiType),* ) -> RetTy;
    
            let module = &$module;
            let name = stringify!($name);

            let require_sig = Sig{
                args: vec!( $( module.get_arg_ty::<$args_t>() ),* ),
                result: module.get_ret_ty::<$ret_t>()
            };
            match module.compile_function(name, require_sig) {
                Ok(raw_ptr) => {
                    let raw_fn: RawFn = unsafe { std::mem::transmute(raw_ptr) };
    
                    Ok(Box::new(move |_state,$($args_e),*| {
                        let res = unsafe { raw_fn( $(ConvertValue::to_bunt(&$args_e)),* ) };
                        let res: $ret_t = ConvertValue::from_bunt(res);
                        res
                    }))
                }
                Err(err) => Err(err)
            }
        }
    };
}

macro_rules! bunt_define {
    ($module:expr, fn $name:ident( $( $args_e:ident: $args_t:ty ),* ) $body:block) => {
        bunt_define!($module, fn $name( $( $args_e: $args_t ),* ) -> () $body)
    };
    ($module:expr, fn $name:ident( $( $args_e:ident: $args_t:ty ),* ) -> $ret_t:ty $body:block) => {
        {
            use $crate::type_convert::ConvertValue;
            use $crate::front::Sig;
    
            type RetTy = <$ret_t as ConvertValue>::AbiType;
            type RawFn = unsafe extern "C" fn( $(<$args_t as ConvertValue>::AbiType),* ) -> RetTy;
    
            let module = &$module;
            let name = stringify!($name);

            let require_sig = Sig{
                args: vec!( $( module.get_arg_ty::<$args_t>() ),* ),
                result: module.get_ret_ty::<$ret_t>()
            };

            fn inner($( $args_e: $args_t ),*) -> $ret_t {
                $body
            }

            unsafe extern "C" fn wrapper($( $args_e: <$args_t as ConvertValue>::AbiType ),*) -> RetTy {
                let res: $ret_t = inner( $(ConvertValue::from_bunt($args_e)),* );
                let res: RetTy = ConvertValue::to_bunt(&res);
                res
            }
            let wrapper_ptr: RawFn = wrapper;

            module.define_function(name,require_sig,wrapper_ptr as *const u8)
        }
    }
}
