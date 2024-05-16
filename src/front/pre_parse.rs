use std::{collections::HashMap, ops::Range};

use crate::{errors::{CompileError, CompileErrorKind}, front::lexer::TokenKind};

use super::lexer::{SourceFile, Token};


pub fn pre_parse(file: &SourceFile) -> Result<PreParse, CompileError> {
    
    let mut parser = Parser{
        tokens: file.tokens(),
        index: 0,
    };

    let mut result = PreParse::default();

    loop {
        let token = parser.next();
        match token.kind {
            TokenKind::KeyFn => {
                let name = parser.expect_ident()?;
                
                let sig_range = parser.skip_until(|kind| match kind {
                    TokenKind::OpenCurlyBrace(_) => true,
                    _ => false
                }).map_err(|token| {
                    CompileError{
                        kind: CompileErrorKind::ParseError,
                        message: format!("expected identifier, got {:?}",token.kind)
                    }
                })?;

                let body_range = parser.skip_block();

                println!("FN = {} {:?} {:?}",name,sig_range,body_range);
                //dump_tokens(&parser.tokens[sig_range]);
                //println!("=====================");
                //dump_tokens(&parser.tokens[body_range]);
            }
            TokenKind::EOF => {
                return Ok(result)
            }
            _ => return Err(CompileError{
                kind: CompileErrorKind::ParseError,
                message: format!("expected item, got {:?}",token.kind)
            })
        }
    }
}

pub struct Parser<'s> {
    tokens: &'s [Token<'s>],
    index: usize
}

#[derive(Default)]
pub struct PreParse {
    items: HashMap<String, ParseItem>,
}

pub enum ParseItem {
    Function{
        signature: Range<usize>,
        body: Range<usize>
    }
}

impl<'s> Parser<'s> {
    pub fn next(&mut self) -> &Token<'s> {
        if let Some(token) = self.tokens.get(self.index) {
            self.index += 1;
            token
        } else {
            self.tokens.last().unwrap()
        }
    }

    pub fn expect_ident(&mut self) -> Result<&'s str,CompileError> {
        let token = self.next();
        match token.kind {
            TokenKind::Ident(name) => {
                Ok(name)
            }
            _ => {
                Err(CompileError{
                    kind: CompileErrorKind::ParseError,
                    message: format!("expected identifier, got {:?}",token.kind)
                })
            }
        }
    }

    pub fn skip_until(&mut self, f: impl Fn(TokenKind)->bool) -> Result<Range<usize>,&Token> {
        // TODO should we skip parens?
        let start = self.index;

        while let Some(token) = self.tokens.get(self.index) {
            if f(token.kind) {
                return Ok(start..self.index);
            }

            self.index += 1;
        }

        Err(self.tokens.last().unwrap())
    }

    /// Must be used on a block. Will panic otherwise.
    pub fn skip_block(&mut self) -> Range<usize> {
        let start = self.index;
        let t = &self.tokens[self.index];
        match t.kind {
            TokenKind::OpenParen(n) |
            TokenKind::OpenCurlyBrace(n) |
            TokenKind::OpenSquareBracket(n) => {
                self.index = n+1;
                start..self.index
            }
            _ => panic!("current token is not a block")
        }
    }
}

pub fn dump_tokens(tokens: &[Token]) {
    for token in tokens {
        println!("> {:?}",token);
    }
}
