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
    pub block: Block<'a>,
    pub exprs: HandleVec<Expr<'a>>,
    pub vars: HandleVec<Var<'a>>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub result: Option<ExprHandle<'a>>,
}

pub type ExprHandle<'a> = Handle<Expr<'a>>;

#[derive(Debug)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub ty: Type<'a>,
    pub span: Range<u32>,
}

#[derive(Debug)]
pub enum ExprKind<'a> {
    Var(VarHandle<'a>),
    Number(f64),
    Bool(bool),
    BinOp(ExprHandle<'a>, BinOp, ExprHandle<'a>),
    If {
        cond: ExprHandle<'a>,
        block_then: Box<Block<'a>>,
        block_else: Option<Box<Block<'a>>>,
    },
}

pub type VarHandle<'a> = Handle<Var<'a>>;

#[derive(Debug)]
pub struct Var<'a> {
    pub ty: Type<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    NotEq,
}

impl<'a> FunctionBody<'a> {
    pub fn parse(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        let block = Block::parse_no_scope(parser)?;

        Ok(Self {
            block,
            exprs: std::mem::take(&mut parser.exprs),
            vars: std::mem::take(&mut parser.vars),
        })
    }
}

impl<'a> Block<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        parser.scopes.open();
        let res = Self::parse_no_scope(parser);
        parser.scopes.close();
        res
    }

    fn parse_no_scope(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        parser.expect(Token::OpCurlyBraceOpen)?;
        let expr = parse_expr(parser, 0)?;
        parser.expect(Token::OpCurlyBraceClose)?;
        Ok(Self { result: Some(expr) })
    }
}

fn parse_expr<'a>(parser: &mut Parser<'a>, min_bp: u8) -> Result<ExprHandle<'a>, CompileError> {
    let front = parser.source.front;

    let mut lhs = match parser.next() {
        Token::Ident => {
            let name = parser.slice();
            let var = parser.scopes.get(name)?;
            let ty = parser.vars.get(var).ty;

            parser.exprs.alloc(Expr {
                kind: ExprKind::Var(var),
                ty,
                span: parser.span(),
            })
        }
        Token::Number => {
            let number = parser.slice().parse::<f64>().map_err(|_| CompileError {
                kind: CompileErrorKind::SyntaxError,
                message: format!("failed to parse number"),
            })?;

            parser.exprs.alloc(Expr {
                kind: ExprKind::Number(number),
                ty: front.common_types().number,
                span: parser.span(),
            })
        }
        t @ (Token::KeyTrue | Token::KeyFalse) => parser.exprs.alloc(Expr {
            kind: ExprKind::Bool(t == Token::KeyTrue),
            ty: front.common_types().bool,
            span: parser.span(),
        }),
        Token::OpParenOpen => {
            let inner = parse_expr(parser, 0)?;
            parser.expect(Token::OpParenClose)?;
            inner
        }
        Token::KeyIf => {
            let span = parser.span();

            let cond = parse_expr(parser, 0)?;
            let block_then = Box::new(Block::parse(parser)?);

            let (block_else, type_else) = if parser.peek() == Token::KeyElse {
                parser.next();

                let block = Box::new(Block::parse(parser)?);
                let ty = get_block_type(parser, &block);

                (Some(block), ty)
            } else {
                let void = parser.source.front.common_types().void;
                (None, void)
            };

            let type_then = get_block_type(parser, &block_then);

            let ty = get_common_type(parser, type_then, type_else)?;

            parser.exprs.alloc(Expr {
                kind: ExprKind::If {
                    cond,
                    block_then,
                    block_else,
                },
                ty,
                span,
            })
        }
        _ => return Err(parser.error("expression")),
    };
    /*let lhs_span = parser.span();

        parser.exprs.alloc(Expr {
            kind: lhs_kind,
            ty: lhs_ty,
            span: lhs_span,
        })
    };*/

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

        lhs = parser.exprs.alloc(Expr {
            kind: ExprKind::BinOp(lhs, op, rhs),
            ty: get_infix_ty(parser, lhs, op, rhs)?,
            span: op_span,
        });
    }

    Ok(lhs)
}

fn get_infix_op(token: Token) -> Option<(BinOp, u8, u8)> {
    match token {
        Token::OpEq => Some((BinOp::Eq, 5, 6)),
        Token::OpNotEq => Some((BinOp::NotEq, 5, 6)),
        Token::OpGt => Some((BinOp::Gt, 5, 6)),

        Token::OpAdd => Some((BinOp::Add, 11, 12)),
        Token::OpSub => Some((BinOp::Sub, 11, 12)),

        Token::OpMul => Some((BinOp::Mul, 13, 14)),
        Token::OpDiv => Some((BinOp::Div, 13, 14)),

        _ => None,
    }
}

fn get_infix_ty<'a>(
    parser: &Parser<'a>,
    lhs: ExprHandle<'a>,
    op: BinOp,
    rhs: ExprHandle<'a>,
) -> Result<Type<'a>, CompileError> {
    let lhs = parser.exprs.get(lhs).ty;
    let rhs = parser.exprs.get(rhs).ty;

    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
            if lhs.kind == TypeKind::Number && rhs.kind == TypeKind::Number {
                return Ok(lhs);
            }
        }
        BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
            let bool = parser.source.front.common_types().number;
            return Ok(bool);
        }
    }

    Err(CompileError {
        kind: CompileErrorKind::TypeError,
        message: format!("could not type operator"),
    })
}

fn get_block_type<'a>(parser: &Parser<'a>, block: &Block<'a>) -> Type<'a> {
    if let Some(res) = block.result {
        parser.exprs.get(res).ty
    } else {
        parser.source.front.common_types().void
    }
}

fn get_common_type<'a>(
    parser: &Parser<'a>,
    a: Type<'a>,
    b: Type<'a>,
) -> Result<Type<'a>, CompileError> {
    if a == b {
        Ok(a)
    } else {
        Err(CompileError {
            kind: CompileErrorKind::TypeError,
            message: format!("could not get common type for {:?} and {:?}", a, b),
        })
    }
}
