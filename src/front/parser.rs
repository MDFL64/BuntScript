use logos::Source;

use crate::errors::{CompileError, CompileErrorKind};

use super::{
    lexer::{Token, TokenInfo},
    FrontEnd,
};

pub struct Parser<'a> {
    source: &'a str,
    tokens: &'a [TokenInfo],
    index: usize,

    pub front: &'a FrontEnd<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &'a [TokenInfo], front: &'a FrontEnd<'a>) -> Self {
        Self {
            source,
            tokens,
            index: usize::MAX,
            front,
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
            None => Token::EOF,
        }
    }

    fn get(&self) -> &'a TokenInfo {
        match self.tokens.get(self.index) {
            Some(res) => res,
            None => self.tokens.last().unwrap(),
        }
    }

    pub fn slice(&self) -> &'a str {
        let span = self.get().span.clone();
        let span = (span.start as usize)..(span.end as usize);
        self.source.slice(span).unwrap_or("")
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
}

#[derive(PartialEq)]
enum BracketKind {
    Paren,
    CurlyBrace,
    SquareBracket,
}
