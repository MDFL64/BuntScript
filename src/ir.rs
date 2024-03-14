use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
};

use cranelift_module::FuncId;
use logos::Span;
use typed_arena::Arena;

use crate::{
    handle_vec::{Handle, HandleVec},
    single_pass::{ScopeStack, ScopeValue},
    types::{InternedType, Sig, Type, TypeKind},
};

/// A Bunt program without the public API or state information
pub struct RawProgram<'vm> {
    pub modules: RefCell<HandleVec<Module<'vm>>>,
    pub stmts: Arena<StmtData<'vm>>,
    pub exprs: Arena<ExprData<'vm>>,

    types: Arena<InternedType>,
    type_intern_map: RefCell<HashMap<TypeKind, Type<'vm>>>,
    common_types: OnceCell<CommonTypes<'vm>>, //compiler: ProgramCompiler
}

impl<'vm> RawProgram<'vm> {
    pub fn new() -> Self {
        Self {
            modules: Default::default(),
            stmts: Default::default(),
            exprs: Default::default(),

            types: Arena::new(),
            type_intern_map: Default::default(),
            common_types: OnceCell::new(),
        }
    }

    pub fn common_types(&'vm self) -> &CommonTypes<'vm> {
        self.common_types.get_or_init(|| CommonTypes {
            number: self.get_type(&TypeKind::Number),
        })
    }

    pub fn get_type(&'vm self, key: &TypeKind) -> Type<'vm> {
        let mut type_intern_map = self.type_intern_map.borrow_mut();

        if let Some(ty) = type_intern_map.get(&key) {
            *ty
        } else {
            let interned = self.types.alloc(InternedType::Known(key.clone()));
            let ty = Type::new(interned);
            type_intern_map.insert(key.clone(), ty);
            ty
        }
    }

    pub fn alloc_type_var(&'vm self) -> Type<'vm> {
        let interned = self.types.alloc(InternedType::Variable);
        Type::new(interned)
    }
}

pub struct CommonTypes<'vm> {
    pub number: Type<'vm>,
}

pub type ModuleHandle<'vm> = Handle<Module<'vm>>;

pub struct Module<'vm> {
    items: HashMap<String, ScopeValue<'vm>>,
}

impl<'vm> Module<'vm> {
    pub fn new(items: HashMap<String, ScopeValue<'vm>>) -> Self {
        Self { items }
    }

    pub fn get(&self, name: &str) -> Option<&ScopeValue<'vm>> {
        self.items.get(name)
    }
}

pub struct Function<'vm> {
    pub name: String,
    pub arg_count: usize,
    pub ret_ty: Option<Type<'vm>>,
    pub body: Block<'vm>,
    pub clif_id: OnceCell<FuncId>,
    vars: HandleVec<Var<'vm>>,
}

pub type VarHandle<'vm> = Handle<Var<'vm>>;

#[derive(Debug)]
pub struct Var<'vm> {
    pub ty: Type<'vm>,
    pub name: String,
}

impl<'vm> Function<'vm> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            arg_count: 0,
            ret_ty: None,
            body: Block { stmts: Vec::new() },
            vars: HandleVec::default(),
            clif_id: OnceCell::new(),
        }
    }

    pub fn sig(&self) -> Sig {
        let mut args = Vec::with_capacity(self.arg_count);

        for var in self.vars.iter() {
            if args.len() >= self.arg_count {
                break;
            }
            args.push(var.ty);
        }

        Sig {
            args,
            result: self.ret_ty.unwrap(),
        }
    }

    pub fn declare_var(
        &mut self,
        name: &str,
        ty: Type<'vm>,
        scopes: &mut ScopeStack<'vm>,
    ) -> VarHandle {
        let name = name.to_owned();

        let var = self.vars.alloc(Var {
            ty,
            name: name.clone(),
        });

        scopes.declare(name, ScopeValue::Local(var));

        var
    }

    pub fn get_var(&self, handle: VarHandle<'vm>) -> &Var<'vm> {
        self.vars.get(handle)
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = &Var> {
        self.vars.iter()
    }
}

pub struct Block<'vm> {
    pub stmts: Vec<Stmt<'vm>>,
}

pub type Stmt<'vm> = &'vm StmtData<'vm>;
pub type Expr<'vm> = &'vm ExprData<'vm>;

pub struct StmtData<'vm> {
    pub kind: StmtKind<'vm>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ExprData<'vm> {
    pub kind: ExprKind<'vm>,
    pub span: Span,
    pub ty: Type<'vm>,
}

pub enum StmtKind<'vm> {
    Return(Option<Expr<'vm>>),
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'vm> {
    LitNumber(f64),
    Local(VarHandle<'vm>),
    BinaryOp(Expr<'vm>, BinaryOp, Expr<'vm>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
}
