// ============= START TYPES =============

use std::{borrow::Borrow, cell::OnceCell, collections::HashMap};

use cranelift_module::FuncId;

use crate::{
    front::CompileError,
    handle_vec::{Handle, HandleVec},
    types::Sig,
};

pub type ExprHandle = Handle<Expr>;
pub type VarHandle = Handle<Var>;

pub struct Module {
    /// Must be unique within a program.
    pub name: Symbol,
    pub items: HashMap<Symbol, Function>,
}

pub struct Function {
    pub name: Symbol,
    pub syn_args: Vec<(Symbol, Type)>,
    pub syn_ret_ty: Type,
    pub body: Block,
    pub vars: HandleVec<Var>,
    pub exprs: HandleVec<Expr>,
    pub sig: OnceCell<Sig>,
    pub is_checked: bool,
    pub clif_id: OnceCell<FuncId>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprHandle),
    // kinda big compared to the other variant
    Let {
        name: Symbol,
        resolved_var: OnceCell<VarHandle>,
        syn_ty: Option<Type>,
        init: Option<ExprHandle>,
    },
    Assign(ExprHandle, ExprHandle),

    Block(Block),
    If(ExprHandle, Block, Option<Block>),
    While(ExprHandle, Block),
    Return(Option<ExprHandle>),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
    pub pos: u32,
}

#[derive(Debug)]
pub enum ExprKind {
    Number(f64),
    Binary(ExprHandle, BinOp, ExprHandle),

    Local(VarHandle),

    // replaced during checking
    Ident(Symbol),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Lt,
    Gt,
    LtEq,
    GtEq,
}

pub enum OpKind {
    Arithmetic,
    Ordinal,
    Equality,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    Void,
    Error,
}

pub struct Var {
    pub ty: Type,
}

// ============= END TYPES =============

impl Module {
    pub fn new(items: Vec<Function>) -> Self {
        let items = items
            .into_iter()
            .map(|func| (func.name.clone(), func))
            .collect();
        Self {
            name: Symbol::new("_mod"),
            items,
        }
    }
}

impl Function {
    pub fn new(
        name: Symbol,
        syn_args: Vec<(Symbol, Type)>,
        syn_ret_ty: Type,
        exprs: HandleVec<Expr>,
        body: Block,
    ) -> Self {
        Self {
            name,
            syn_args,
            syn_ret_ty,
            sig: OnceCell::new(),
            exprs,
            body,
            vars: Default::default(),
            is_checked: false,
            clif_id: OnceCell::new(),
        }
    }

    pub fn dump(&self) {
        println!("{:?} :: {:?}", self.name, self.sig.get());

        for (i, expr) in self.exprs.iter().enumerate() {
            println!("{:5} = {:?} :: {:?}", i, expr.kind, expr.ty);
        }
        println!("---");
        for (i, var) in self.vars.iter().enumerate() {
            println!("{:5} :: {:?}", i, var.ty);
        }
    }
}

impl Symbol {
    pub fn new(val: &str) -> Self {
        Self(val.to_owned())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Stmt {
    pub fn new_let(name: Symbol, syn_ty: Option<Type>, init: Option<ExprHandle>) -> Self {
        Stmt::Let {
            name,
            syn_ty,
            init,
            resolved_var: OnceCell::new(),
        }
    }
}

impl Type {
    pub fn is_number(&self) -> bool {
        match self {
            Type::Number => true,
            _ => false,
        }
    }

    pub fn is_valid(&self) -> bool {
        match self {
            Type::Error => false,
            _ => true,
        }
    }

    pub fn unify(self, other: Type) -> Result<Type, CompileError> {
        if self == other {
            Ok(self.clone())
        } else {
            /*match (self, other) {
                (ty, Type::Unknown) => Ok(ty),
                (Type::Unknown, ty) => Ok(ty),

                _ => Err(CheckError {})
            }*/
            panic!()
        }
    }
}

impl BinOp {
    pub fn kind(&self) -> OpKind {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => OpKind::Arithmetic,
            BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => OpKind::Ordinal,
        }
    }
}

impl Borrow<str> for Symbol {
    fn borrow(&self) -> &str {
        &self.0
    }
}
