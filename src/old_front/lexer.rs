use logos::{Lexer as LexerInternal, Logos, Span};

use crate::errors::{CompileError, CompileErrorKind, CompileErrorSource};
use std::path::PathBuf;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\r\n\f]+")] // skip whitespace
pub enum Token {
    #[token("fn")]
    KeyFunction,
    #[token("return")]
    KeyReturn,

    #[token("(")]
    OpLParen,
    #[token(")")]
    OpRParen,
    #[token("{")]
    OpLCurly,
    #[token("}")]
    OpRCurly,
    #[token("[")]
    OpLSquare,
    #[token("]")]
    OpRSquare,

    #[token(".")]
    OpDot,
    #[token(",")]
    OpComma,
    #[token(":")]
    OpColon,
    #[token(";")]
    OpSemi,
    #[token("->")]
    OpArrow,

    #[token("+")]
    OpPlus,

    #[regex("[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident,

    #[regex("[0-9]+")]
    Number,
}

pub struct Lexer<'a> {
    source_file: PathBuf,
    lexer: LexerInternal<'a, Token>,
    // contains the NEXT peeked token and the CURRENT span
    peeked: Option<Result<Token, ()>>,
}

// high level
impl<'a> Lexer<'a> {
    pub fn new(source_file: PathBuf, source: &'a str) -> Self {
        Self {
            source_file,
            lexer: Token::lexer(source),
            peeked: None,
        }
    }

    pub fn expect(&mut self, expected: Token, expected_name: &str) -> Result<(), CompileError> {
        let token = self.next()?;
        if token != expected {
            self.error_expect(expected_name)
        } else {
            Ok(())
        }
    }

    pub fn check(&mut self, expected: Token) -> bool {
        if self.peek() == Some(Ok(expected)) {
            let _ = self.next();
            true
        } else {
            false
        }
    }

    pub fn expect_ident(&mut self) -> Result<&'a str, CompileError> {
        self.expect(Token::Ident, "identifier")
            .map(|_| self.lexer.slice())
    }
}

// low level
impl<'a> Lexer<'a> {
    pub fn maybe_next(&mut self) -> Option<Result<Token, CompileError>> {
        let token = if let Some(peeked) = self.peeked.take() {
            peeked
        } else {
            self.lexer.next()?
        };

        Some(token.map_err(|_| {
            let err_string = format!("invalid token '{}'", self.lexer.slice());
            self.error(CompileErrorKind::ParseError(err_string))
        }))
    }

    pub fn next(&mut self) -> Result<Token, CompileError> {
        match self.maybe_next() {
            Some(res) => res,
            None => {
                let err_string = "unexpected end of input".to_owned();
                Err(self.error(CompileErrorKind::ParseError(err_string)))
            }
        }
    }

    pub fn peek(&mut self) -> Option<Result<Token, CompileError>> {
        let peeked = match self.peeked {
            Some(peeked) => Some(peeked),
            None => {
                self.peeked = self.lexer.next();
                self.peeked
            }
        };

        peeked.map(|res| {
            res.map_err(|_| {
                let err_string = format!("invalid token '{}'", self.lexer.slice());
                self.error(CompileErrorKind::ParseError(err_string))
            })
        })
    }

    pub fn token_str(&self) -> &'a str {
        self.lexer.slice()
    }

    pub fn token_span(&self) -> Span {
        self.lexer.span()
    }
}

// errors
impl<'a> Lexer<'a> {
    pub fn error(&self, kind: CompileErrorKind) -> CompileError {
        CompileError {
            kind,
            source: CompileErrorSource::Bunt {
                file: self.source_file.clone(),
                span: self.lexer.span(),
            },
        }
    }

    pub fn error_expect<T>(&self, expected: &str) -> Result<T, CompileError> {
        let current = self.lexer.slice();

        Err(self.error(CompileErrorKind::ParseError(format!(
            "expected {}, got '{}'",
            expected, current
        ))))
    }
}
