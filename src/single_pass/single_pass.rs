use std::{collections::HashMap, path::PathBuf};

use logos::Span;

use crate::{
    checker::Checker,
    errors::{CompileError, CompileErrorKind},
    ir::{
        BinaryOp, Block, Expr, ExprData, ExprKind, Function, Module, ModuleHandle, RawProgram,
        Stmt, StmtData, StmtKind,
    },
    types::Type,
};

use super::{
    lexer::{Lexer, Token},
    scopes::{ScopeStack, ScopeValue},
};

pub struct SinglePass<'vm, 'source> {
    lexer: Lexer<'source>,
    scopes: ScopeStack<'vm>,
    program: &'vm RawProgram<'vm>,
    active_function: Option<Function<'vm>>,
}

impl<'vm, 'source> SinglePass<'vm, 'source> {
    pub fn compile(
        source: &str,
        source_name: PathBuf,
        program: &'vm RawProgram<'vm>,
    ) -> Result<ModuleHandle<'vm>, CompileError> {
        let mut compiler = SinglePass {
            lexer: Lexer::new(source_name, source),
            scopes: ScopeStack::default(),
            program,
            active_function: None,
        };

        let items = compiler.parse_items()?;

        let mut modules = program.modules.borrow_mut();
        let mod_id = modules.alloc(Module::new("UNIT".to_owned(),items));

        Ok(mod_id)
    }

    fn parse_items(&mut self) -> Result<HashMap<String, ScopeValue<'vm>>, CompileError> {
        self.scopes.open();

        while let Some(token) = self.lexer.maybe_next() {
            match token? {
                Token::KeyFunction => {
                    let fn_name = self.lexer.expect_ident()?;

                    let mut func: Function<'vm> = Function::new(fn_name.to_owned());

                    // scope for the body of the function
                    // do not open an extra scope for the body block!
                    self.scopes.open();

                    // arg list
                    self.parse_list(Token::OpLParen, |this| {
                        let arg_name = this.lexer.expect_ident()?;
                        // type annotations are not optional here
                        let Some(arg_ty) = this.try_parse_type_annotation()? else {
                            return this.lexer.error_expect("type");
                        };

                        func.declare_var(arg_name, arg_ty, &mut this.scopes);
                        func.arg_count += 1;

                        Ok(())
                    })?;

                    let ret_ty = self.try_parse_type_annotation()?;

                    func.ret_ty = Some(match ret_ty {
                        Some(ty) => ty,
                        None => self.program.alloc_type_var(),
                    });

                    assert!(self.active_function.is_none());
                    self.active_function = Some(func);

                    let body = self.parse_block_no_scope()?;

                    self.scopes.close();

                    let mut func = self.active_function.take().unwrap();
                    func.body = body;

                    self.scopes
                        .declare(func.name.clone(), ScopeValue::Function(func));
                }
                _ => {
                    return self.lexer.error_expect("item");
                }
            }
        }

        let mod_scope = self.scopes.close();
        Ok(mod_scope)
    }

    fn parse_block_no_scope(&mut self) -> Result<Block<'vm>, CompileError> {
        self.lexer.expect(Token::OpLCurly, "'{'")?;

        let mut stmts = Vec::new();

        while !self.lexer.check(Token::OpRCurly) {
            stmts.push(self.parse_stmt()?);
        }

        Ok(Block { stmts })
    }

    fn parse_stmt(&mut self) -> Result<Stmt<'vm>, CompileError> {
        let token = self.lexer.peek();
        let span = self.lexer.token_span();

        if let Some(Ok(token)) = token {
            match token {
                Token::KeyReturn => {
                    let _ = self.lexer.next();

                    if !self.lexer.check(Token::OpSemi) {
                        let expr = self.parse_expr(0)?;

                        return Ok(self.alloc_stmt(StmtKind::Return(Some(expr)), span));
                    } else {
                        return Ok(self.alloc_stmt(StmtKind::Return(None), span));
                    }
                }
                _ => (),
            }
        }
        self.lexer.error_expect("statement")
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr<'vm>, CompileError> {
        let lhs = match self.lexer.next()? {
            Token::Ident => {
                let name = self.lexer.token_str();

                let value = self.scopes.get(name).map_err(|err| self.lexer.error(err))?;

                match value {
                    ScopeValue::Local(var) => ExprKind::Local(*var),
                    ScopeValue::Function(_) => {
                        return Err(self.lexer.error(CompileErrorKind::NotYetImplemented(
                            "function ref".to_owned(),
                        )))
                    }
                }
            }
            Token::Number => {
                let number = self.lexer.token_str().parse::<f64>().map_err(|_| {
                    self.lexer.error(CompileErrorKind::ParseError(
                        "failed to parse number".to_owned(),
                    ))
                })?;

                ExprKind::LitNumber(number)
            }
            _ => return self.lexer.error_expect("expression"),
        };
        let span = self.lexer.token_span();

        let mut lhs = self.alloc_expr(lhs, span)?;

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

            lhs = self.alloc_expr(ExprKind::BinaryOp(lhs, op, rhs), op_span)?;
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

    fn try_parse_type_annotation(&mut self) -> Result<Option<Type<'vm>>, CompileError> {
        if self.lexer.check(Token::OpColon) {
            self.parse_type().map(|ty| Some(ty))
        } else {
            Ok(None)
        }
    }

    fn parse_type(&mut self) -> Result<Type<'vm>, CompileError> {
        let token = self.lexer.next()?;
        match token {
            Token::Ident => {
                let name = self.lexer.token_str();
                let res = match name {
                    "number" => self.program.common_types().number,
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

    fn alloc_expr(&self, kind: ExprKind<'vm>, span: Span) -> Result<Expr<'vm>, CompileError> {
        let ty = match kind {
            ExprKind::Local(var) => {
                let func = self.active_function.as_ref().unwrap();
                func.get_var(var).ty
            }
            ExprKind::LitNumber(_) => self.program.common_types().number,
            ExprKind::BinaryOp(lhs, op, rhs) => {
                let lhs_ty = lhs.ty;
                let rhs_ty = rhs.ty;

                self.infer_op_bin(lhs_ty, op, rhs_ty)?
            }
        };

        Ok(self.program.exprs.alloc(ExprData { kind, span, ty }))
    }

    fn alloc_stmt(&self, kind: StmtKind<'vm>, span: Span) -> Stmt<'vm> {
        self.program.stmts.alloc(StmtData { kind, span })
    }

    pub fn infer_op_bin(
        &self,
        lhs: Type<'vm>,
        op: BinaryOp,
        rhs: Type<'vm>,
    ) -> Result<Type<'vm>, CompileError> {
        let ty = Checker::solve_op_bin(lhs, op, rhs)?.unwrap_or_else(|| {
            panic!("todo add constraint");
        });

        Ok(ty)
    }
}
