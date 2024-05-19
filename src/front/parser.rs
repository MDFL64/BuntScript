use logos::Source;

use crate::errors::{CompileError, CompileErrorKind};

use super::lexer::{Token, TokenInfo};

pub struct Parser<'a,'t> {
    source: &'a str,
    tokens: &'t [TokenInfo],
    index: usize
}

impl<'a,'t> Parser<'a,'t> {
    pub fn new(source: &'a str, tokens: &'t [TokenInfo]) -> Self {
        Self {
            source,
            tokens,
            index: usize::MAX
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn next(&mut self) -> Token {
        self.index = self.index.wrapping_add(1);

        self.get().kind
    }

    pub fn peek(&self) -> Token {
        let index = self.index.wrapping_add(1);
        match self.tokens.get(index) {
            Some(res) => res.kind,
            None => Token::EOF
        }
    }

    fn get(&self) -> &'t TokenInfo {
        match self.tokens.get(self.index) {
            Some(res) => res,
            None => self.tokens.last().unwrap()
        }
    }

    pub fn slice(&self) -> &'a str {
        let span = self.get().span.clone();
        let span = (span.start as usize)..(span.end as usize);
        self.source.slice(span).unwrap_or("")
    }

    pub fn error(&self, expected: &str) -> CompileError {
        let slice = self.slice();

        CompileError{
            kind: CompileErrorKind::SyntaxError,
            message: format!("expected {}, got '{}'",expected,slice)
        }
    }

    pub fn expect(&mut self, kind: Token) -> Result<(), CompileError> {
        let present = self.next();
        if present == kind {
            Ok(())
        } else {
            Err(self.error(&format!("{:?}",kind)))
        }
    }

    pub fn expect_ident(&mut self) -> Result<&'a str, CompileError> {
        match self.next() {
            Token::Ident => {
                Ok(self.slice())
            }
            _ => Err(self.error("identifier"))
        }
    }
}
