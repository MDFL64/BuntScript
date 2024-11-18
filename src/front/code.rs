use std::ops::Range;

use crate::{
    errors::{CompileError, CompileErrorKind},
    front::lexer::Token,
    handle_vec::{Handle, HandleVec},
};

use super::{
    parser::Parser,
    scopes::ScopeItem,
    types::{Type, TypeKind},
    Function,
};

#[derive(Debug)]
pub struct FunctionBody<'a> {
    pub block: Block<'a>,
    pub exprs: HandleVec<Expr<'a>>,
    pub vars: HandleVec<Var>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
    pub result: Option<ExprHandle<'a>>,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Let(VarHandle<'a>, ExprHandle<'a>),
    Expr(ExprHandle<'a>),
}

pub type ExprHandle<'a> = Handle<Expr<'a>>;

#[derive(Debug)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub ty: Type,
    pub span: Range<u32>,
}

#[derive(Debug)]
pub enum ExprKind<'a> {
    Var(VarHandle<'a>),
    FuncRef(&'a Function<'a>),
    Number(f64),
    Bool(bool),
    BinOp(ExprHandle<'a>, BinOp, ExprHandle<'a>),
    Call(ExprHandle<'a>, Box<[ExprHandle<'a>]>),
    If {
        cond: ExprHandle<'a>,
        expr_then: ExprHandle<'a>,
        expr_else: Option<ExprHandle<'a>>,
    },
    While {
        cond: ExprHandle<'a>,
        body: ExprHandle<'a>,
    },
    Block(Box<Block<'a>>),
}

pub type VarHandle<'a> = Handle<Var>;

#[derive(Debug)]
pub struct Var {
    pub ty: Type,
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

    Assign,
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

        let mut stmts = Vec::new();

        let result = loop {
            let next = parser.peek();
            match next {
                Token::KeyLet => {
                    parser.next();
                    // TODO support proper patterns here
                    let name = parser.expect_ident()?;

                    // TODO type annotations?
                    // TODO do we want to support let's that don't assign?
                    parser.expect(Token::OpAssign)?;
                    let expr = parse_expr(parser, 0)?;
                    parser.expect(Token::OpSemi)?;

                    let ty = parser.exprs.get(expr).ty.clone();

                    let var = parser.declare_var(name, ty);

                    stmts.push(Stmt::Let(var, expr));
                }
                Token::OpCurlyBraceClose => {
                    parser.next();
                    break None;
                }
                Token::OpSemi => {
                    // skip empty statements
                    parser.next();
                }
                _ => {
                    // when treating control structures as statements, the trailing semicolon may be omitted
                    // TODO require type = void for these expressions?
                    let can_skip_semi = match next {
                        Token::KeyIf | Token::KeyWhile => true,
                        _ => false,
                    };

                    let expr = parse_expr(parser, 0)?;
                    let next = parser.next();
                    match next {
                        Token::OpSemi => {
                            stmts.push(Stmt::Expr(expr));
                        }
                        Token::OpCurlyBraceClose => {
                            break Some(expr);
                        }
                        _ => {
                            if can_skip_semi {
                                stmts.push(Stmt::Expr(expr));
                                parser.back();
                            } else {
                                return Err(parser.error("';' or '}'"));
                            }
                        }
                    }
                }
            }
        };

        let ty = match result {
            Some(expr) => parser.exprs.get(expr).ty.clone(),
            None => Type::void(),
        };

        Ok(Self { stmts, result, ty })
    }
}

fn parse_expr<'a>(parser: &mut Parser<'a>, min_bp: u8) -> Result<ExprHandle<'a>, CompileError> {
    let mut lhs = match parser.next() {
        Token::Ident => {
            let name = parser.slice();
            let item = parser.scopes.get(name)?;

            match item {
                ScopeItem::Var(var) => {
                    let ty = parser.vars.get(var).ty.clone();

                    parser.exprs.alloc(Expr {
                        kind: ExprKind::Var(var),
                        ty,
                        span: parser.span(),
                    })
                }
                ScopeItem::Item(func) => {
                    let sig = func.sig()?;

                    parser.exprs.alloc(Expr {
                        kind: ExprKind::FuncRef(func),
                        ty: sig.ty_sig.to_fn_type(),
                        span: parser.span(),
                    })
                }
            }
        }
        Token::Number => {
            let number = parser.slice().parse::<f64>().map_err(|_| CompileError {
                kind: CompileErrorKind::SyntaxError,
                message: format!("failed to parse number"),
            })?;

            parser.exprs.alloc(Expr {
                kind: ExprKind::Number(number),
                ty: Type::Number,
                span: parser.span(),
            })
        }
        t @ (Token::KeyTrue | Token::KeyFalse) => parser.exprs.alloc(Expr {
            kind: ExprKind::Bool(t == Token::KeyTrue),
            ty: Type::Bool,
            span: parser.span(),
        }),
        Token::OpParenOpen => {
            let inner = parse_expr(parser, 0)?;
            parser.expect(Token::OpParenClose)?;
            inner
        }
        Token::OpCurlyBraceOpen => {
            let span = parser.span();
            parser.back();

            let block = Box::new(Block::parse(parser)?);
            let ty = block.ty.clone();

            parser.exprs.alloc(Expr {
                kind: ExprKind::Block(block),
                ty,
                span,
            })
        }
        Token::KeyIf => {
            let span = parser.span();

            let cond = parse_expr(parser, 0)?;

            if parser.peek() != Token::OpCurlyBraceOpen {
                parser.next();
                return Err(parser.error("block"));
            }

            let expr_then = parse_expr(parser, 0)?;
            let type_then = parser.exprs.get(expr_then).ty.clone();

            let (expr_else, type_else) = if parser.peek() == Token::KeyElse {
                parser.next();

                let peeked = parser.peek();
                if peeked != Token::OpCurlyBraceOpen && peeked != Token::KeyIf {
                    parser.next();
                    return Err(parser.error("block or if"));
                }

                let expr = parse_expr(parser, 0)?;
                let ty = parser.exprs.get(expr).ty.clone();

                (Some(expr), ty)
            } else {
                let void = Type::void();
                (None, void)
            };

            let ty = get_common_type(parser, &type_then, &type_else)?;

            parser.exprs.alloc(Expr {
                kind: ExprKind::If {
                    cond,
                    expr_then,
                    expr_else,
                },
                ty,
                span,
            })
        }
        Token::KeyWhile => {
            let span = parser.span();

            let cond = parse_expr(parser, 0)?;

            if parser.peek() != Token::OpCurlyBraceOpen {
                parser.next();
                return Err(parser.error("block"));
            }

            let body = parse_expr(parser, 0)?;

            parser.exprs.alloc(Expr {
                kind: ExprKind::While { cond, body },
                ty: Type::void(),
                span,
            })
        }
        _ => return Err(parser.error("expression")),
    };

    // postfix ops
    match parser.peek() {
        Token::OpParenOpen => {
            parser.next();
            let span = parser.span();

            // parse arg list
            let mut args = Vec::new();
            loop {
                if parser.peek() == Token::OpParenClose {
                    parser.next();
                    break;
                }
                let arg = parse_expr(parser, 0)?;
                args.push(arg);

                match parser.next() {
                    Token::OpParenClose => break,
                    Token::OpComma => (),
                    _ => return Err(parser.error("',' or ')'")),
                }
            }

            let args = args.into_boxed_slice();
            let func_ty = &parser.exprs.get(lhs).ty;
            let res_ty = func_ty.fn_result().map_err(|_| CompileError {
                kind: CompileErrorKind::TypeError,
                message: "attempt to call non-function".to_owned(),
            })?;

            lhs = parser.exprs.alloc(Expr {
                kind: ExprKind::Call(lhs, args),
                ty: res_ty,
                span,
            });
        }
        _ => (),
    }

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
        Token::OpAssign => Some((BinOp::Assign, 2, 1)),

        Token::OpEq => Some((BinOp::Eq, 5, 6)),
        Token::OpNotEq => Some((BinOp::NotEq, 5, 6)),
        Token::OpGt => Some((BinOp::Gt, 5, 6)),
        Token::OpLt => Some((BinOp::Lt, 5, 6)),

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
) -> Result<Type, CompileError> {
    let lhs = &parser.exprs.get(lhs).ty;
    let rhs = &parser.exprs.get(rhs).ty;

    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
            if lhs == &Type::Number && rhs == &Type::Number {
                return Ok(lhs.clone());
            }
        }
        BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
            return Ok(Type::Bool);
        }
        BinOp::Assign => {
            let void = Type::void();
            return Ok(void);
        }
    }

    Err(CompileError {
        kind: CompileErrorKind::TypeError,
        message: format!("could not type operator"),
    })
}

fn get_common_type<'a>(parser: &Parser<'a>, a: &Type, b: &Type) -> Result<Type, CompileError> {
    if a == b {
        Ok(a.clone())
    } else {
        Err(CompileError {
            kind: CompileErrorKind::TypeError,
            message: format!("could not get common type for {:?} and {:?}", a, b),
        })
    }
}
