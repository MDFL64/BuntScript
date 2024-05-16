use self_cell::self_cell;
use std::{path::PathBuf, str::CharIndices};

use crate::errors::{CompileError, CompileErrorKind};

pub struct SourceFile {
    path: PathBuf,
    lexed: LexedSource,
}

self_cell! {
    pub struct LexedSource {
        owner: String,
        #[covariant]
        dependent: Tokens,
    }
}

type Tokens<'s> = Vec<Token<'s>>;

#[derive(Debug, Copy, Clone)]
pub struct SourceLoc {
    pub index: u32,
    pub line: u32,
}

impl SourceLoc {
    /// Invalid location denoted by line=0. Used for 0 byte files.
    pub const INVALID: Self = Self {
        index: 0,
        line: 0
    };
}

#[derive(Debug)]
pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind<'s> {
    Invalid,
    EOF,

    KeyFn,

    Ident(&'s str),
    Number(f64),

    OpComma,
    OpColon,

    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpRem,

    OpGreaterThan,
    OpArrow,

    OpenParen(usize),
    OpenCurlyBrace(usize),
    OpenSquareBracket(usize),

    CloseParen,
    CloseCurlyBrace,
    CloseSquareBracket,
}

enum BraceKind {
    Paren,
    CurlyBrace,
    SquareBracket
}

impl SourceFile {
    pub fn new(path: PathBuf, source: String) -> Result<Self, CompileError> {
        Ok(Self {
            path,
            lexed: LexedSource::try_new(source, lex)?,
        })
    }

    pub fn tokens(&self) -> &[Token] {
        self.lexed.borrow_dependent()
    }
}

fn lex<'s>(source_str: &'s String) -> Result<Tokens<'s>, CompileError> {
    let mut tokens = Vec::new();

    let mut braces = Vec::new();

    let mut source = LexIterator::new(source_str).peekable();

    while let Some((start_i, c, start)) = source.next() {
        macro_rules! push_token {
            ($kind:expr) => {
                tokens.push(Token {
                    kind: $kind,
                    loc: start
                })
            };
        }

        match c {
            // space
            ' ' | '\n' => {
                // skip
            }
            // identifiers and keywords
            '_' | 'a'..='z' | 'A'..='Z' => {
                let mut end_i = start_i;
                while let Some((i, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9', loc)) = source.peek() {
                    end_i = *i;
                    source.next();
                }

                let token_string = &source_str[start_i..=end_i];

                let token_kind = match token_string {
                    "fn" => TokenKind::KeyFn,
                    _ => TokenKind::Ident(token_string)
                };

                push_token!(token_kind);
            }
            // numbers - todo allow leading decimals, underscores, hex/bin/octal, and scientific notation
            '0'..='9' => {
                let mut end_i = start_i;
                while let Some((i, '.' | '0'..='9', loc)) = source.peek() {
                    end_i = *i;
                    source.next();
                }

                let token_string = &source_str[start_i..=end_i];
                let n = token_string.parse::<f64>().map_err(|e| {
                    CompileError {
                        kind: CompileErrorKind::ParseError,
                        message: "failed to parse number".to_owned(),
                    }
                })?;

                tokens.push(Token {
                    kind: TokenKind::Number(n),
                    loc: start
                });
            }
            ':' => push_token!(TokenKind::OpColon),
            ',' => push_token!(TokenKind::OpComma),
            '+' => push_token!(TokenKind::OpAdd),
            '-' => {
                let next = source.peek().map(|(_,c,_)| *c);
                match next {
                    Some('>') => {
                        source.next();
                        push_token!(TokenKind::OpArrow);
                    }
                    _ => push_token!(TokenKind::OpSub)
                }
            }
            '*' => push_token!(TokenKind::OpMul),
            '/' => push_token!(TokenKind::OpDiv),
            '%' => push_token!(TokenKind::OpRem),

            '>' => push_token!(TokenKind::OpGreaterThan),
            '(' => {
                braces.push((tokens.len(),BraceKind::Paren));
                push_token!(TokenKind::Invalid);
            }
            '{' => {
                braces.push((tokens.len(),BraceKind::CurlyBrace));
                push_token!(TokenKind::Invalid);
            }
            ')' => {
                let popped = braces.pop();
                if let Some((i,BraceKind::Paren)) = popped {
                    tokens[i].kind = TokenKind::OpenParen(tokens.len());
                    push_token!(TokenKind::CloseParen);
                } else {
                    return Err(CompileError {
                        kind: CompileErrorKind::ParseError,
                        message: "bracket mismatch".to_owned(),
                    })
                }
            }
            '}' => {
                let popped = braces.pop();
                if let Some((i,BraceKind::CurlyBrace)) = popped {
                    tokens[i].kind = TokenKind::OpenCurlyBrace(tokens.len());
                    push_token!(TokenKind::CloseCurlyBrace);
                } else {
                    return Err(CompileError {
                        kind: CompileErrorKind::ParseError,
                        message: "bracket mismatch".to_owned(),
                    })
                }
            }
            _ => return Err(CompileError {
                kind: CompileErrorKind::ParseError,
                message: format!("lexer fail: [{}]",c),
            })
        }
    }

    {
        let last_loc = if source_str.len() > 0 {
            let last_line = tokens.last().map(|t| t.loc.line).unwrap_or(1);
            SourceLoc{
                index: source_str.len() as u32 - 1,
                line: last_line
            }
        } else {
            SourceLoc::INVALID
        };

        tokens.push(Token {
            kind: TokenKind::EOF,
            loc: last_loc
        });
    }

    Ok(tokens)
}

struct LexIterator<'a> {
    inner: CharIndices<'a>,
    next_line: u32,
}

impl<'a> LexIterator<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: source.char_indices(),
            next_line: 1,
        }
    }
}

impl<'a> Iterator for LexIterator<'a> {
    type Item = (usize, char, SourceLoc);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((i, c)) = self.inner.next() {
            let res = (i, c, SourceLoc{
                index: i as u32,
                line: self.next_line
            });
            if c == '\n' {
                self.next_line += 1;
            }

            Some(res)
        } else {
            None
        }
    }
}
