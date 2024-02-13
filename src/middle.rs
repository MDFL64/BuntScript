// ============= START TYPES =============

use std::cell::OnceCell;

use crate::{checker::CheckError, handle_vec::{Handle, HandleVec}, types::Sig};

pub type ExprHandle = Handle<Expr>;
pub type VarHandle = Handle<Var>;

pub struct Function {
    pub syn_args: Vec<(Symbol, Type)>,
    pub root: ExprHandle,
    pub vars: HandleVec<Var>,
    pub exprs: HandleVec<Expr>,
    pub sig: OnceCell<Sig>
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprHandle),
    // kinda big compared to the other variant
    Let{
        name: Symbol,
        resolved_var: OnceCell<VarHandle>,
        syn_ty: Option<Type>,
        init: Option<ExprHandle>
    }
}

pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
    pub pos: u32
}

#[derive(Debug)]
pub enum ExprKind {
    Block{
        stmts: Vec<Stmt>,
        result: Option<ExprHandle>
    },

    Number(f64),
    Binary(ExprHandle, BinOp, ExprHandle),
    Assign(ExprHandle, ExprHandle),

    Local(VarHandle),

    Return(Option<ExprHandle>),

    If(ExprHandle, ExprHandle, Option<ExprHandle>),

    // only exists in front
    Ident(Symbol),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    Void,
    Unknown,
    Never,
}

pub struct Var {
    pub ty: Type
}

// ============= END TYPES =============

impl Function {
    pub fn new(syn_args: Vec<(Symbol, Type)>, exprs: HandleVec<Expr>, root: ExprHandle) -> Self {
        Self {
            syn_args,
            exprs,
            root,
            vars: Default::default(),
            sig: OnceCell::new()
        }
    }

    pub fn dump(&self) {
        println!("sig = {:?}", self.sig);
        for (i, expr) in self.exprs.iter().enumerate() {
            println!("{:5} = {:?} :: {:?}", i, expr.kind, expr.ty);
        }
        println!("---");
        for (i,var) in self.vars.iter().enumerate() {
            println!("{:5} :: {:?}", i, var.ty);
        }
    }
}

impl Symbol {
    pub fn new(val: &str) -> Self {
        Self(val.to_owned())
    }
}

impl Stmt {
    pub fn new_let(name: Symbol, syn_ty: Option<Type>, init: Option<ExprHandle>) -> Self {
        Stmt::Let { name, syn_ty, init, resolved_var: OnceCell::new() }
    }
}

impl Type {
    pub fn is_number(&self) -> bool {
        match self {
            Type::Number => true,
            _ => false,
        }
    }

    pub fn is_never(&self) -> bool {
        match self {
            Type::Never => true,
            _ => false,
        }
    }

    pub fn is_known(&self) -> bool {
        match self {
            Type::Unknown => false,
            _ => true,
        }
    }

    /// Used in the backend to assert `self`` exactly matches `other`, or one is `never`
    pub fn is(self, other: Type) -> bool {
        self == other || self == Type::Never || other == Type::Never
    }

    pub fn unify(self, other: Type) -> Result<Type,CheckError> {
        if self == other {
            Ok(self.clone())
        } else {
            match (self, other) {
                (ty, Type::Unknown) => Ok(ty),
                (Type::Unknown, ty) => Ok(ty),

                (ty, Type::Never) => Ok(ty),
                (Type::Never, ty) => Ok(ty),

                _ => Err(CheckError {})
            }
        }
    }

    pub fn sum(self, other: Type) -> Result<Type,CheckError> {
        if self == other {
            Ok(self.clone())
        } else {
            match (self, other) {
                (ty, Type::Unknown) => Ok(ty),
                (Type::Unknown, ty) => Ok(ty),

                (ty, Type::Never) => Ok(ty),
                (Type::Never, ty) => Ok(ty),

                _ => Err(CheckError {})
            }
        }
    }
}
