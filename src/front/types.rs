use crate::{
    errors::{CompileError, CompileErrorKind},
    front::lexer::Token,
};

use super::parser::Parser;

pub type Type<'a> = &'a InternedType<'a>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct InternedType<'a> {
    pub kind: TypeKind<'a>,
    // NOTE: if more fields are added, we may want to exclude them from Eq / Hash?
    // ALTERNATIVELY, consider adding a type id and comparing using it
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeKind<'a> {
    Number,
    Bool,
    String,
    Tuple(Vec<Type<'a>>),
}

/// A function signature which includes named arguments.
#[derive(Debug, PartialEq)]
pub struct Sig<'a> {
    pub args: Vec<Type<'a>>,
    pub result: Type<'a>,
}

pub fn parse_type<'a>(parser: &mut Parser<'a>) -> Result<Type<'a>, CompileError> {
    let front = parser.source.front;

    let token = parser.next();
    match token {
        Token::Ident => match parser.slice() {
            "number" => Ok(front.common_types().number),
            "bool" => Ok(front.common_types().bool),
            name => Err(CompileError {
                kind: CompileErrorKind::CanNotResolve,
                message: format!("cannot resolve type name: '{}'", name),
            }),
        },
        _ => Err(parser.error("type")),
    }
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
