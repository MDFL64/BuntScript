use std::ops::Range;

use crate::handle_vec::{Handle, HandleVec};

#[derive(Debug)]
pub struct FunctionBody<'a> {
    body: Block<'a>,
    exprs: HandleVec<Expr<'a>>
}

#[derive(Debug)]
struct Block<'a> {
    result: Option<Expr<'a>>
}

type ExprHandle<'a> = Handle<Expr<'a>>;

#[derive(Debug)]
struct Expr<'a> {
    span: Range<u32>,
    kind: ExprKind<'a>
}

#[derive(Debug)]
enum ExprKind<'a> {
    Ident(&'a str),
    Number(f64),
    BinOp(ExprHandle<'a>,BinOp,ExprHandle<'a>)
}

#[derive(Debug)]
enum BinOp {
    Add
}
