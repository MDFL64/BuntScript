use std::{collections::HashMap, path::PathBuf};

use logos::Span;

use crate::{
    errors::{CompileError, CompileErrorKind}, front::ir::Function, handle_vec::HandleVec
};

use super::{ir::{BinaryOp, Expr, ExprHandle, ExprKind, StmtKind}, lexer::{Lexer, Token}};
use super::ir::{Module, Item, Block, Stmt, SynType};

pub struct Parser<'s> {
    lexer: Lexer<'s>,
    new_exprs: HandleVec<Expr<'s>>
}

impl<'s> Parser<'s> {
    pub fn parse_module(
        source: &'s str,
        source_name: PathBuf,
    ) -> Result<Module<'s>, CompileError> {
        let mut parser = Self {
            lexer: Lexer::new(source_name, source),
            new_exprs: HandleVec::default()
        };

        let items = parser.parse_items()?;

        Ok(Module {
            items
        })
    }

    fn parse_items(&mut self) -> Result<HashMap<&'s str, Item<'s>>, CompileError> {
        let mut items = HashMap::new();

        while let Some(token) = self.lexer.maybe_next() {
            match token? {
                Token::KeyFunction => {
                    let fn_name = self.lexer.expect_ident()?;

                    let mut args = Vec::new();
                    self.parse_list(Token::OpLParen, |this| {
                        let arg_name = this.lexer.expect_ident()?;
                        // type annotations are not optional here
                        let Some(arg_ty) = this.try_parse_type_annotation()? else {
                            return this.lexer.error_expect("type");
                        };

                        args.push((arg_name, arg_ty));

                        Ok(())
                    })?;

                    self.lexer.expect(Token::OpArrow,"'->'")?;

                    let ret_ty = self.parse_type()?;

                    assert!(self.new_exprs.len() == 0);

                    let body = self.parse_block()?;
                    let span = body.span.clone();
                    let body = self.alloc_expr(ExprKind::Block(body), span);

                    let func = Function{
                        args,
                        ret_ty,
                        exprs: std::mem::take(&mut self.new_exprs),
                        body
                    };

                    let old = items.insert(fn_name, Item::Function(func));
                    assert!(old.is_none());
                }
                _ => {
                    return self.lexer.error_expect("item");
                }
            }
        }

        Ok(items)
    }

    fn parse_block(&mut self) -> Result<Block<'s>, CompileError> {
        self.lexer.expect(Token::OpLCurly, "'{'")?;
        let start = self.lexer.token_span();

        let mut stmts = Vec::new();

        while !self.lexer.check(Token::OpRCurly) {
            stmts.push(self.parse_stmt()?);
        }

        let end = self.lexer.token_span();
        Ok(Block {
            stmts,
            result: None,
            span: start.start .. end.end
        })
    }

    fn parse_stmt(&mut self) -> Result<Stmt<'s>, CompileError> {
        let token = self.lexer.peek();
        let span = self.lexer.token_span();

        if let Some(Ok(token)) = token {
            match token {
                Token::KeyReturn => {
                    let _ = self.lexer.next();

                    let ret_val = if !self.lexer.check(Token::OpSemi) {
                        let expr = self.parse_expr(0)?;
                        self.lexer.expect(Token::OpSemi, "';'")?;

                        Some(expr)
                    } else {
                        None
                    };
                    return Ok(Stmt {
                        kind: StmtKind::Return(ret_val),
                        span
                    });
                }
                _ => (),
            }
        }
        self.lexer.error_expect("statement")
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<ExprHandle<'s>, CompileError> {
        let lhs = match self.lexer.next()? {
            Token::Ident => {
                let name = self.lexer.token_str();
                ExprKind::Ident(name)
            }
            Token::Number => {
                let number = self.lexer.token_str().parse::<f64>().map_err(|_| {
                    self.lexer.error(CompileErrorKind::ParseError(
                        "failed to parse number".to_owned(),
                    ))
                })?;
                ExprKind::Number(number)
            }
            _ => return self.lexer.error_expect("expression"),
        };
        let span = self.lexer.token_span();

        let mut lhs = self.alloc_expr(lhs, span);

        loop {
            let (op, lhs_bp, rhs_bp) = match self.lexer.peek() {
                Some(Ok(token)) => {
                    if let Some(op_info) = Self::get_infix_op(token) {
                        op_info
                    } else {
                        break;
                    }
                }
                _ => break,
            };

            if lhs_bp < min_bp {
                break;
            }

            // commit to using this op
            let _ = self.lexer.next();
            let op_span = self.lexer.token_span();

            let rhs = self.parse_expr(rhs_bp)?;

            lhs = self.alloc_expr(ExprKind::BinaryOp(lhs, op, rhs), op_span);
        }

        Ok(lhs)
    }

    /// returns (op,lhs_bp,rhs_bp)
    fn get_infix_op(token: Token) -> Option<(BinaryOp, u8, u8)> {
        match token {
            Token::OpPlus => Some((BinaryOp::Add, 1, 2)),
            _ => None,
        }
    }

    fn try_parse_type_annotation(&mut self) -> Result<Option<SynType>, CompileError> {
        if self.lexer.check(Token::OpColon) {
            self.parse_type().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_type(&mut self) -> Result<SynType, CompileError> {
        let token = self.lexer.next()?;
        match token {
            Token::Ident => {
                let name = self.lexer.token_str();
                let res = match name {
                    "number" => SynType::Number,
                    _ => panic!("nyi type {}", name),
                };
                return Ok(res);
            }
            _ => (),
        }

        self.lexer.error_expect("type")
    }

    fn parse_list(
        &mut self,
        start_bracket: Token,
        mut f: impl FnMut(&mut Self) -> Result<(), CompileError>,
    ) -> Result<(), CompileError> {
        let (end_bracket, start_str, end_str) = match start_bracket {
            Token::OpLParen => (Token::OpRParen, "'('", "')'"),
            Token::OpLCurly => (Token::OpRCurly, "'{'", "'}'"),
            Token::OpLSquare => (Token::OpRSquare, "'['", "']'"),
            _ => panic!("invalid bracket"),
        };

        self.lexer.expect(start_bracket, start_str)?;

        // first item or end bracket
        if self.lexer.peek() != Some(Ok(end_bracket)) {
            loop {
                // item
                f(self)?;

                match self.lexer.next()? {
                    Token::OpComma => {
                        // allow trailing commas
                        if self.lexer.peek() == Some(Ok(end_bracket)) {
                            let _ = self.lexer.next();
                            break;
                        }
                    }
                    t if t == end_bracket => break,
                    _ => return self.lexer.error_expect(&format!("',' or {}", end_str)),
                }
            }
        }

        Ok(())
    }

    fn alloc_expr(&mut self, kind: ExprKind<'s>, span: Span) -> ExprHandle<'s> {
        self.new_exprs.alloc(Expr { kind, span })
    }
}
