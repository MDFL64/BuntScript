use std::{cell::OnceCell, collections::HashMap, ops::Range};

use crate::{
    errors::{CompileError, CompileErrorKind},
    front::lexer::Token,
    util::get_or_try_init,
};

use super::{
    code::FunctionBody,
    front::SourceFile,
    lexer::TokenInfo,
    parser::Parser,
    types::{parse_type, Sig, Type},
};

pub struct ModuleItems<'a> {
    table: HashMap<&'a str, &'a Function<'a>>,
}

pub struct Function<'a> {
    sig_slice: &'a [TokenInfo],
    body_slice: &'a [TokenInfo],

    sig_parsed: OnceCell<SigPair<'a>>,
    body_parsed: OnceCell<FunctionBody<'a>>,

    pub source: &'a SourceFile<'a>,
}

#[derive(Debug)]
pub struct SigPair<'a> {
    pub ty_sig: Sig<'a>,
    pub arg_names: Vec<&'a str>,
}

impl<'a> ModuleItems<'a> {
    pub fn parse(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        let mut table = HashMap::new();

        let front = parser.source.front;

        loop {
            match parser.next() {
                Token::KeyFn => {
                    let name = parser.expect_ident()?;

                    let sig_slice = parser.skip_until(Token::OpCurlyBraceOpen)?;
                    let body_slice = parser.skip_brackets()?;

                    let func = front.alloc_function(Function {
                        sig_slice,
                        body_slice,
                        sig_parsed: OnceCell::new(),
                        body_parsed: OnceCell::new(),
                        source: parser.source,
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

    pub fn get(&self, name: &str) -> Option<&'a Function<'a>> {
        self.table.get(name).copied()
    }
}

impl<'a> Function<'a> {
    pub fn sig(&self) -> Result<&SigPair<'a>, CompileError> {
        get_or_try_init(&self.sig_parsed, || {
            let mut parser = Parser::new(self.source, &self.sig_slice)?;

            SigPair::parse(&mut parser)
        })
    }

    pub fn body(&self) -> Result<&FunctionBody<'a>, CompileError> {
        get_or_try_init(&self.body_parsed, || {
            let mut parser = Parser::new(self.source, &self.body_slice)?;

            parser.scopes.open();

            let sig = self.sig()?;
            for (arg_name, arg_ty) in sig.arg_names.iter().zip(&sig.ty_sig.args) {
                parser.declare_var(arg_name, arg_ty);
            }

            let res = FunctionBody::parse(&mut parser);

            parser.scopes.close();

            res
        })
    }
}

impl<'a> SigPair<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, CompileError> {
        parser.expect(Token::OpParenOpen)?;

        let mut arg_names = Vec::new();
        let mut arg_tys = Vec::new();

        loop {
            if parser.peek() == Token::OpParenClose {
                break;
            }

            {
                let arg_name = parser.expect_ident()?;
                parser.expect(Token::OpColon)?;

                let ty = parse_type(parser)?;

                arg_names.push(arg_name);
                arg_tys.push(ty);
            }

            match parser.next() {
                Token::OpComma => (),
                Token::OpParenClose => break,
                _ => return Err(parser.error("',' or ')'")),
            }
        }

        let result = if parser.peek() == Token::OpArrow {
            parser.next();

            parse_type(parser)?
        } else {
            panic!("no return type");
        };

        Ok(Self {
            arg_names,
            ty_sig: Sig {
                args: arg_tys,
                result,
            },
        })
    }
}
