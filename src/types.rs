use crate::middle::Type;

#[derive(Debug)]
pub struct Sig {
    pub args: Vec<Type>,
    pub result: Type
}
