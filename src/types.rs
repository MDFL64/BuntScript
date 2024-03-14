
#[derive(Debug, PartialEq)]
pub struct Sig<'vm> {
    pub args: Vec<Type<'vm>>,
    pub result: Type<'vm>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Type<'vm> {
    interned: &'vm InternedType
}

impl<'vm> Type<'vm> {
    pub fn new(interned: &'vm InternedType) -> Self {
        Self { interned }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InternedType {
    Known(TypeKind),
    Variable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Number,
    Bool,

    // todo alias with null and undefined
    Void
}
