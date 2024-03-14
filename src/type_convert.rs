use crate::{ir::RawProgram, types::Type};

pub trait ArgValue {
    fn bunt_type<'vm>(program: &'vm RawProgram<'vm>) -> Type<'vm>;

    type AbiType;

    fn to_bunt(&self) -> Self::AbiType;

    fn from_bunt(val: Self::AbiType) -> Self;
}

pub trait RetValue: ArgValue {}

impl ArgValue for f64 {
    fn bunt_type<'vm>(program: &'vm RawProgram<'vm>) -> Type<'vm> {
        program.common_types().number
    }

    type AbiType = f64;

    fn to_bunt(&self) -> Self::AbiType {
        *self
    }

    fn from_bunt(val: Self::AbiType) -> Self {
        val
    }
}

impl RetValue for f64 {}
