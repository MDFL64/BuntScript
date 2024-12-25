use logos::Logos;

use std::ops::Range;

use crate::errors::{CompileError, CompileErrorKind};

#[derive(Logos, Debug, Clone, Copy, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")] // skip whitespace
pub enum Token {
    #[token("fn")]
    KeyFn,
    #[token("let")]
    KeyLet,
    #[token("if")]
    KeyIf,
    #[token("else")]
    KeyElse,
    #[token("while")]
    KeyWhile,
    #[token("extern")]
    KeyExtern,

    #[token("true")]
    KeyTrue,
    #[token("false")]
    KeyFalse,

    #[token("(")]
    OpParenOpen,
    #[token(")")]
    OpParenClose,

    #[token("{")]
    OpCurlyBraceOpen,
    #[token("}")]
    OpCurlyBraceClose,

    #[token("[")]
    OpSquareBracketOpen,
    #[token("]")]
    OpSquareBracketClose,

    #[token("->")]
    OpArrow,
    #[token(":")]
    OpColon,
    #[token(";")]
    OpSemi,
    #[token(",")]
    OpComma,

    #[token("<")]
    OpLt,
    #[token(">")]
    OpGt,
    #[token("<=")]
    OpLtEq,
    #[token(">=")]
    OpGtEq,
    #[token("==")]
    OpEq,
    #[token("!=")]
    OpNotEq,

    #[token("+")]
    OpAdd,
    #[token("-")]
    OpSub,
    #[token("*")]
    OpMul,
    #[token("/")]
    OpDiv,

    #[token("=")]
    OpAssign,

    #[regex("[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident,

    #[regex("([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE]\\-?[0-9]+)?")]
    Number,

    EOF,
}

#[derive(Debug)]
pub struct TokenInfo {
    pub kind: Token,
    pub span: Range<u32>,
}

pub fn lex<'a>(source: &'a String) -> Result<Vec<TokenInfo>, CompileError> {
    // impose a maximum source length, so we can use smaller source spans
    // realistically anything this large will not function well anyway
    if source.len() > 1_000_000_000 {
        return Err(CompileError {
            kind: CompileErrorKind::SyntaxError,
            message: format!("source file is too long"),
        });
    }

    let mut lexer = Token::lexer(source);

    let mut result = Vec::new();
    while let Some(token) = lexer.next() {
        let token = token.map_err(|_| CompileError {
            kind: CompileErrorKind::SyntaxError,
            message: format!("unexpected character: {:?}", lexer.slice()),
        })?;

        let span = lexer.span();
        let span = (span.start as u32)..(span.end as u32);

        result.push(TokenInfo { kind: token, span });
    }

    let len = source.len() as u32;
    result.push(TokenInfo {
        kind: Token::EOF,
        span: len..len,
    });

    Ok(result)
}
