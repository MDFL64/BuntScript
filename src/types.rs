use crate::middle::Type;

#[derive(Debug, PartialEq)]
pub struct Sig {
    pub args: Vec<Type>,
    pub result: Type,
}
