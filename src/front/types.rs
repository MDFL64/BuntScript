use crate::{
    errors::{CompileError, CompileErrorKind},
    front::lexer::Token,
};

use super::parser::Parser;

pub type Type = TypeKind<TypeArgList>;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind<A> {
    Number,
    Bool,
    String,
    Tuple(A),
    Function(A),
}

impl<A> TypeKind<A>
where
    A: Default,
{
    pub fn void() -> Self {
        Self::Tuple(Default::default())
    }
}

impl Type {
    pub fn fn_result(&self) -> Result<Type, ()> {
        if let Type::Function(list) = self {
            Ok(list.list.last().unwrap().clone())
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct TypeArgList {
    list: Vec<Type>,
}

impl TypeArgList {
    pub fn new(list: Vec<Type>) -> Self {
        Self { list }
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn push(&mut self, ty: Type) {
        self.list.push(ty);
    }
}

#[derive(Debug, PartialEq)]
pub struct Sig {
    pub args: Vec<Type>,
    pub result: Type,
}

impl Sig {
    pub fn to_fn_type(&self) -> Type {
        let mut list = TypeArgList::new(self.args.clone());
        list.push(self.result.clone());
        Type::Function(TypeArgList::new(self.args.clone()))
    }
}

pub fn parse_type<'a>(parser: &mut Parser<'a>) -> Result<Type, CompileError> {
    let token = parser.next();
    match token {
        Token::Ident => match parser.slice() {
            "number" => Ok(Type::Number),
            "bool" => Ok(Type::Bool),
            name => Err(CompileError {
                kind: CompileErrorKind::CanNotResolve,
                message: format!("cannot resolve type name: '{}'", name),
            }),
        },
        _ => Err(parser.error("type")),
    }
}
