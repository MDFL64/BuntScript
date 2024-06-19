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
    let common_types = parser.common_types();

    let token = parser.next();
    match token {
        Token::Ident => match parser.slice() {
            "number" => Ok(common_types.number),
            "bool" => Ok(common_types.bool),
            name => Err(CompileError {
                kind: CompileErrorKind::CanNotResolve,
                message: format!("cannot resolve type name: '{}'", name),
            }),
        },
        _ => Err(parser.error("type")),
    }
}
