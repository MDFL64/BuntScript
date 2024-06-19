use std::ops::Range;

use logos::Source;

use crate::{
    errors::{CompileError, CompileErrorKind},
    handle_vec::HandleVec,
};

use super::{
    code::{Expr, Var, VarHandle},
    front::{CommonTypes, Module},
    lexer::{Token, TokenInfo},
    scopes::ScopeStack,
    types::Type, FrontEnd,
};

pub struct Parser<'a> {
    text: &'a str,
    tokens: &'a [TokenInfo],
    index: usize,

    module: &'a Module<'a>,

    // used when building function bodies
    pub exprs: HandleVec<Expr<'a>>,
    pub vars: HandleVec<Var<'a>>,
    pub scopes: ScopeStack<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(module: &'a Module<'a>, tokens: &'a [TokenInfo]) -> Result<Self, CompileError> {
        Ok(Self {
            module,
            text: module.source_text()?,
            tokens,
            index: usize::MAX,

            exprs: HandleVec::default(),
            vars: HandleVec::default(),
            scopes: ScopeStack::new(module),
        })
    }

    pub fn module(&self) -> &'a Module<'a> {
        self.module
    }

    pub fn front(&self) -> &'a FrontEnd<'a> {
        self.module.front()
    }

    pub fn common_types(&self) -> &'a CommonTypes<'a> {
        self.module.front().common_types()
    }

    pub fn next(&mut self) -> Token {
        self.index = self.index.wrapping_add(1);

        self.get().kind
    }

    pub fn peek(&self) -> Token {
        let index = self.index.wrapping_add(1);
        match self.tokens.get(index) {
            Some(res) => res.kind,
            None => Token::EOF,
        }
    }

    pub fn back(&mut self) {
        self.index = self.index.wrapping_sub(1);
    }

    fn get(&self) -> &'a TokenInfo {
        match self.tokens.get(self.index) {
            Some(res) => res,
            None => self.tokens.last().unwrap(),
        }
    }

    pub fn slice(&self) -> &'a str {
        let span = &self.get().span; //.clone();
        let span = (span.start as usize)..(span.end as usize);
        self.text.slice(span).unwrap_or("")
    }

    pub fn span(&self) -> Range<u32> {
        self.get().span.clone()
    }

    pub fn error(&self, expected: &str) -> CompileError {
        let slice = self.slice();

        CompileError {
            kind: CompileErrorKind::SyntaxError,
            message: format!("expected {}, got '{}'", expected, slice),
        }
    }

    pub fn expect(&mut self, kind: Token) -> Result<(), CompileError> {
        let present = self.next();
        if present == kind {
            Ok(())
        } else {
            Err(self.error(&format!("{:?}", kind)))
        }
    }

    pub fn expect_ident(&mut self) -> Result<&'a str, CompileError> {
        match self.next() {
            Token::Ident => Ok(self.slice()),
            _ => Err(self.error("identifier")),
        }
    }

    pub fn skip_until(&mut self, terminal: Token) -> Result<&'a [TokenInfo], CompileError> {
        let base = self.index + 1;

        loop {
            let token = self.next();
            if token == terminal {
                let end = self.index;
                return Ok(&self.tokens[base..end]);
            }
            match token {
                Token::EOF => {
                    return Err(CompileError {
                        kind: CompileErrorKind::SyntaxError,
                        message: format!("unexpected end of input"),
                    });
                }
                Token::OpParenOpen | Token::OpCurlyBraceOpen | Token::OpSquareBracketOpen => {
                    self.skip_brackets()?;
                }
                Token::OpParenClose | Token::OpCurlyBraceClose | Token::OpSquareBracketClose => {
                    return Err(CompileError {
                        kind: CompileErrorKind::SyntaxError,
                        message: format!("unexpected closing bracket: '{}'", self.slice()),
                    });
                }
                _ => (),
            }
        }
    }

    pub fn skip_brackets(&mut self) -> Result<&'a [TokenInfo], CompileError> {
        let base = self.index;
        let base_token = self.get().kind;

        let mut brackets = vec![match base_token {
            Token::OpParenOpen => BracketKind::Paren,
            Token::OpCurlyBraceOpen => BracketKind::CurlyBrace,
            Token::OpSquareBracketOpen => BracketKind::SquareBracket,
            _ => panic!("attempt to skip non-bracket"),
        }];
        loop {
            match self.next() {
                Token::OpParenOpen => brackets.push(BracketKind::Paren),
                Token::OpCurlyBraceOpen => brackets.push(BracketKind::CurlyBrace),
                Token::OpSquareBracketOpen => brackets.push(BracketKind::SquareBracket),

                Token::OpParenClose => {
                    let b = brackets.pop();
                    if b != Some(BracketKind::Paren) {
                        panic!("todo bracket matching errors")
                    }
                    if brackets.len() == 0 {
                        break;
                    }
                }
                Token::OpSquareBracketClose => {
                    let b = brackets.pop();
                    if b != Some(BracketKind::SquareBracket) {
                        panic!("todo bracket matching errors")
                    }
                    if brackets.len() == 0 {
                        break;
                    }
                }
                Token::OpCurlyBraceClose => {
                    let b = brackets.pop();
                    if b != Some(BracketKind::CurlyBrace) {
                        panic!("todo bracket matching errors")
                    }
                    if brackets.len() == 0 {
                        break;
                    }
                }
                Token::EOF => {
                    panic!("todo bracket matching errors")
                }
                _ => (),
            }
        }

        if brackets.len() > 0 {
            panic!("todo bracket matching errors");
        }

        return Ok(&self.tokens[base..self.index + 1]);
    }

    pub fn declare_var(&mut self, name: &'a str, ty: Type<'a>) -> VarHandle<'a> {
        let var = self.vars.alloc(Var { ty });

        self.scopes.declare(name, var);

        var
    }
}

#[derive(PartialEq)]
enum BracketKind {
    Paren,
    CurlyBrace,
    SquareBracket,
}
