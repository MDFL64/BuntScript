use std::ops::Range;

use crate::{
    errors::{CompileError, CompileErrorKind},
    front::lexer::Token,
    handle_vec::{Handle, HandleVec},
};

use super::{
    parser::Parser,
    types::{Type, TypeKind},
};

#[derive(Debug)]
pub struct FunctionBody<'a> {
    body: Block<'a>,
    exprs: HandleVec<Expr<'a>>,
    vars: HandleVec<Var<'a>>,
}

#[derive(Debug)]
struct Block<'a> {
    result: Option<ExprHandle<'a>>,
}

type ExprHandle<'a> = Handle<Expr<'a>>;

#[derive(Debug)]
pub struct Expr<'a> {
    kind: ExprKind<'a>,
    ty: Type<'a>,
    span: Range<u32>,
}

#[derive(Debug)]
enum ExprKind<'a> {
    Var(VarHandle<'a>),
    Number(f64),
    BinOp(ExprHandle<'a>, BinOp, ExprHandle<'a>),
}

pub type VarHandle<'a> = Handle<Var<'a>>;

#[derive(Debug)]
pub struct Var<'a> {
    pub ty: Type<'a>,
}

#[derive(Debug, Clone, Copy)]
enum BinOp {
    Add,
}

impl<'a> FunctionBody<'a> {
    pub fn parse(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        let body = Block::parse_no_scope(parser)?;

        Ok(Self {
            body,
            exprs: std::mem::take(&mut parser.exprs),
            vars: std::mem::take(&mut parser.vars),
        })
    }
}

impl<'a> Block<'a> {
    fn parse_no_scope(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        parser.expect(Token::OpCurlyBraceOpen)?;
        let expr = parse_expr(parser, 0)?;
        parser.expect(Token::OpCurlyBraceClose)?;
        Ok(Self { result: Some(expr) })
    }
}

fn parse_expr<'a>(parser: &mut Parser<'a>, min_bp: u8) -> Result<ExprHandle<'a>, CompileError> {
    let front = parser.source.front;

    let (lhs_kind, lhs_ty) = match parser.next() {
        Token::Ident => {
            let name = parser.slice();
            let var = parser.scopes.get(name)?;
            let ty = parser.vars.get(var).ty;
            (ExprKind::Var(var), ty)
        }
        Token::Number => {
            let number = parser.slice().parse::<f64>().map_err(|_| CompileError {
                kind: CompileErrorKind::SyntaxError,
                message: format!("failed to parse number"),
            })?;
            (ExprKind::Number(number), front.common_types().number)
        }
        _ => return Err(parser.error("expression")),
    };
    let lhs_span = parser.span();

    let mut lhs = parser.exprs.alloc(Expr {
        kind: lhs_kind,
        ty: lhs_ty,
        span: lhs_span,
    });

    loop {
        let next = parser.peek();

        let (op, lhs_bp, rhs_bp) = if let Some(op_info) = get_infix_op(next) {
            op_info
        } else {
            break;
        };

        if lhs_bp < min_bp {
            break;
        }

        // commit to using this op
        let _ = parser.next();
        let op_span = parser.span();

        let rhs = parse_expr(parser, rhs_bp)?;
        let rhs_ty = parser.exprs.get(rhs).ty;

        lhs = parser.exprs.alloc(Expr {
            kind: ExprKind::BinOp(lhs, op, rhs),
            ty: get_infix_ty(parser, lhs_ty, op, rhs_ty)?,
            span: op_span,
        });
    }

    Ok(lhs)
}

fn get_infix_op(token: Token) -> Option<(BinOp, u8, u8)> {
    match token {
        Token::OpAdd => Some((BinOp::Add, 1, 2)),
        _ => None,
    }
}

fn get_infix_ty<'a>(
    parser: &Parser,
    lhs: Type<'a>,
    op: BinOp,
    rhs: Type<'a>,
) -> Result<Type<'a>, CompileError> {
    match op {
        BinOp::Add => {
            if lhs.kind == TypeKind::Number && rhs.kind == TypeKind::Number {
                return Ok(lhs);
            }
        }
    }

    Err(CompileError {
        kind: CompileErrorKind::TypeError,
        message: format!("could not type operator"),
    })
}
