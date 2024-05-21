use std::{cell::OnceCell, collections::HashMap, ops::Range};

use crate::{
    errors::{CompileError, CompileErrorKind},
    front::lexer::Token,
};

use super::{lexer::TokenInfo, parser::Parser, types::Sig};

pub struct ModuleItems<'a> {
    table: HashMap<&'a str, &'a Function<'a>>,
}

#[derive(Debug)]
pub struct Function<'a> {
    sig_slice: &'a [TokenInfo],
    body_slice: &'a [TokenInfo],

    sig_parsed: OnceCell<SigPair<'a>>,
    body_parsed: OnceCell<()>,
    //body: OnceCell<FunctionBody<'a>>
}

#[derive(Debug)]
struct SigPair<'a> {
    ty_sig: Sig<'a>,
    arg_names: Vec<&'a str>,
}

impl<'a> ModuleItems<'a> {
    pub fn parse(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        let mut table = HashMap::new();

        loop {
            match parser.next() {
                Token::KeyFn => {
                    let name = parser.expect_ident()?;

                    let sig_slice = parser.skip_until(Token::OpCurlyBraceOpen)?;
                    let body_slice = parser.skip_brackets()?;

                    let func = parser.front.alloc_function(Function {
                        sig_slice,
                        body_slice,
                        sig_parsed: OnceCell::new(),
                        body_parsed: OnceCell::new(),
                    });

                    let old = table.insert(name, func);

                    if old.is_some() {
                        return Err(CompileError {
                            kind: CompileErrorKind::DuplicateDeclarations,
                            message: format!("symbol '{}' was declared multiple times", name),
                        });
                    }
                }
                Token::EOF => break,
                _ => {
                    return Err(parser.error("item"));
                }
            }
        }

        Ok(Self { table })
    }
}
