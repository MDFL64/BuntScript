use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap
};

use cranelift_module::FuncId;
use logos::Span;
use typed_arena::Arena;

use crate::{
    handle_vec::{Handle, HandleVec}, front::{ScopeStack, ScopeValue}, types::{InternedType, Sig, Type, TypeKind}
};

/// A Bunt program without the public API or state information
pub struct RawProgram<'vm> {
    pub modules: RefCell<HandleVec<Module<'vm>>>,
    pub stmts: Arena<StmtData<'vm>>,
    pub exprs: Arena<ExprData<'vm>>,

    types: Arena<InternedType<'vm>>,
    type_intern_map: RefCell<HashMap<TypeKind, Type<'vm>>>,
    common_types: OnceCell<CommonTypes<'vm>>,
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
        let interned = self.types.alloc(InternedType::Variable(OnceCell::new()));
        Type::new(interned)
    }
}

pub struct CommonTypes<'vm> {
    pub number: Type<'vm>,
}

pub type ModuleHandle<'vm> = Handle<Module<'vm>>;

pub struct Module {
    unique_name: String,
    items: HashMap<String, ScopeValue>,
}

impl<'vm> Module<'vm> {
    pub fn new(unique_name: String, items: HashMap<String, ScopeValue<'vm>>) -> Self {
        Self { unique_name, items }
    }

    pub fn get(&self, name: &str) -> Option<&ScopeValue<'vm>> {
        self.items.get(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str,&ScopeValue<'vm>)> {
        self.items.iter().map(|(k,v)| (k.as_str(),v))
    }

    pub fn unique_name(&self) -> &str {
        &self.unique_name
    }
}

pub struct Function {
    pub name: String,
    pub args: Vec<(String, SynType)>,
    pub ret_ty: SynType,
    pub body: Block,
    pub clif_id: OnceCell<FuncId>
}

impl<'vm> Function<'vm> {
    pub fn new(name: String, args: Vec<SynType>, ret_ty: SynType, scopes: &mut ScopeStack<'vm>) -> Self {
        let func = Self {
            name,
            arg_count: args.len(),
            ret_ty,
            body: Block { stmts: Vec::new() },
            vars: HandleVec::from_vec(args),
            clif_id: OnceCell::new(),
        };

        for (key,var) in func.vars.iter() {
            scopes.declare(var.name.clone(), ScopeValue::Local(key));
        }

        func
    }

    pub fn sig(&self) -> Sig<'vm> {
        let mut args = Vec::with_capacity(self.arg_count);

        for (_,var) in self.vars.iter() {
            if args.len() >= self.arg_count {
                break;
            }
            args.push(var.ty.unwrap());
        }

        Sig {
            args,
            result: self.ret_ty
        }
    }

    pub fn declare_var(
        &mut self,
        name: String,
        scopes: &mut ScopeStack<'vm>,
    ) -> VarHandle<'vm> {
        let var = self.vars.alloc(Var {
            ty: None,
            name: name.clone(),
        });

        scopes.declare(name, ScopeValue::Local(var));

        var
    }

    pub fn get_var(&self, handle: VarHandle<'vm>) -> &Var<'vm> {
        self.vars.get(handle)
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = (VarHandle<'vm>,&Var<'vm>)> {
        self.vars.iter()
    }
}

pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Option<Expr>
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

#[derive(Debug)]
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
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    LtEq,
    GtEq
}

pub enum OpKind {
    Arithmetic,
    Ordinal,
    Equality,
}

impl BinaryOp {
    pub fn kind(&self) -> OpKind {
        match self {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => OpKind::Arithmetic,
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => OpKind::Ordinal,
        }
    }
}
