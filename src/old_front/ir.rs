use std::collections::HashMap;

use logos::Span;

use crate::handle_vec::{Handle, HandleVec};

pub struct Module<'s> {
    pub items: HashMap<&'s str, Item<'s>>
}

pub enum Item<'s> {
    Function(Function<'s>)
}

pub struct Function<'s> {
    pub args: Vec<(&'s str, SynType)>,
    pub ret_ty: SynType,
    pub exprs: HandleVec<Expr<'s>>,
    pub body: ExprHandle<'s>
}

pub enum SynType {
    Number,
    Bool,
    String
}

pub type ExprHandle<'s> = Handle<Expr<'s>>;

pub struct Expr<'s> {
    pub kind: ExprKind<'s>,
    pub span: Span
}

pub struct Stmt<'s> {
    pub kind: StmtKind<'s>,
    pub span: Span
}

pub enum ExprKind<'s> {
    Number(f64),
    Ident(&'s str),
    BinaryOp(ExprHandle<'s>, BinaryOp, ExprHandle<'s>),
    Block(Block<'s>),
}

pub struct Block<'s> {
    pub stmts: Vec<Stmt<'s>>,
    pub result: Option<ExprHandle<'s>>,
    pub span: Span
}

pub enum StmtKind<'s> {
    Expr(ExprHandle<'s>),
    Return(Option<ExprHandle<'s>>)
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
