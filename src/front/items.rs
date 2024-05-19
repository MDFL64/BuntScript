use std::{collections::HashMap, ops::Range};

use crate::{errors::{CompileError, CompileErrorKind}, front::lexer::Token};

use super::{parser::Parser, types::SigF};

pub struct ModuleF<'a> {
    items: HashMap<&'a str, FunctionF<'a>>
}

pub struct FunctionF<'a> {
    sig: SigF<'a>,
    body_range: Range<usize>
}

impl<'a> ModuleF<'a> {
    pub fn parse<'t>(parser: &mut Parser<'a,'t>) -> Result<Self,CompileError> {
        let mut items = HashMap::new();

        loop {
            match parser.next() {
                Token::KeyFn => {
                    let name = parser.expect_ident()?;
                    let sig = SigF::parse(parser)?;

                    // block pre-parsing
                    let body_range = {
                        parser.expect(Token::OpCurlyBraceOpen)?;
                        let block_start = parser.index();

                        let mut brackets = vec!(BracketKind::CurlyBrace);

                        loop {
                            match parser.next() {
                                Token::OpParenOpen => brackets.push(BracketKind::Paren),
                                Token::OpCurlyBraceOpen => brackets.push(BracketKind::CurlyBrace),

                                Token::OpParenClose => {
                                    let b = brackets.pop();
                                    if b != Some(BracketKind::Paren) {
                                        panic!("todo bracket matching errors")
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
                                _ => ()
                            }
                        }
                        let block_end = parser.index();
                        block_start..block_end
                    };

                    let old = items.insert(name, FunctionF{
                        sig,
                        body_range
                    });

                    if old.is_some() {
                        return Err(CompileError {
                            kind: CompileErrorKind::DuplicateDeclarations,
                            message: format!("symbol '{}' was declared multiple times",name)
                        });
                    }
                }
                Token::EOF => break,
                _ => {
                    return Err(parser.error("item"));
                }
            }
        }

        Ok(Self{
            items: Default::default()
        })
    }
}

#[derive(PartialEq)]
enum BracketKind {
    Paren,
    CurlyBrace,
    SquareBracket
}
