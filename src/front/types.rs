use crate::{errors::CompileError, front::lexer::Token};

use super::parser::Parser;

pub type Type<'a> = &'a InternedType<'a>;

#[derive(Debug)]
pub struct InternedType<'a> {
    kind: TypeKind<'a>,
}

#[derive(Debug)]
pub enum TypeKind<'a> {
    Number,
    String,
    Tuple(Vec<Type<'a>>),
}

/// A function signature which includes named arguments.
#[derive(Debug)]
pub struct Sig<'a> {
    pub args: Vec<Type<'a>>,
    pub result: Type<'a>,
}

/*impl<'a> Type<'a> {
    pub fn parse<'t>(parser: &mut Parser<'a,'t>) -> Result<Self,CompileError> {
        let kind = parser.next();
        match kind {
            Token::Ident => Ok(TypeF::Named(parser.slice())),
            _ => Err(parser.error("type"))
        }
    }
}

impl<'a> Sig<'a> {
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

                let ty = Type::parse(parser)?;

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

            Type::parse(parser)?
        } else {
            panic!("no return type");
        };

        Ok(Self {
            args,
            result
        })
    }
}*/
