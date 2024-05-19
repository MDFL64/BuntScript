use crate::{errors::CompileError, front::lexer::Token};

use super::parser::Parser;


#[derive(Debug)]
pub enum TypeF<'a> {
    Named(&'a str),
    Tuple(Vec<TypeF<'a>>)
}

/// A function signature which includes named arguments.
#[derive(Debug)]
pub struct SigF<'a> {
    pub args: Vec<(&'a str,TypeF<'a>)>,
    pub result: TypeF<'a>
}

impl<'a> TypeF<'a> {
    pub fn parse<'t>(parser: &mut Parser<'a,'t>) -> Result<Self,CompileError> {
        let kind = parser.next();
        match kind {
            Token::Ident => Ok(TypeF::Named(parser.slice())),
            _ => Err(parser.error("type"))
        }
    }
}

impl<'a> SigF<'a> {
    pub fn parse<'t>(parser: &mut Parser<'a,'t>) -> Result<Self,CompileError> {
        parser.expect(Token::OpParenOpen)?;

        let mut args = Vec::new();

        loop {
            if parser.peek() == Token::OpParenClose {
                break;
            }

            //println!("-->");
            // name: type
            {
                let arg_name = parser.expect_ident()?;
                parser.expect(Token::OpColon)?;

                let ty = TypeF::parse(parser)?;

                args.push((arg_name,ty));
            }

            match parser.next() {
                Token::OpComma => (),
                Token::OpParenClose => break,
                _ => return Err(parser.error("',' or ')'"))
            }
        }
        
        let result = if parser.peek() == Token::OpArrow {
            parser.next();

            TypeF::parse(parser)?
        } else {
            panic!("no return type");
        };

        Ok(Self {
            args,
            result
        })
    }
}
