use crate::middle::Type;

pub trait ArgValue {
    fn bunt_type() -> Type;

    type AbiType;

    fn to_bunt(&self) -> Self::AbiType;

    fn from_bunt(val: Self::AbiType) -> Self;
}

pub trait RetValue: ArgValue {}

impl ArgValue for f64 {
    fn bunt_type() -> Type {
        Type::Number
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
