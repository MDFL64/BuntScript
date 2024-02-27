use crate::middle::Type;

pub trait ToBuntValue {
    fn bunt_type() -> Type;

    type AbiType;

    fn to_abi(&self) -> Self::AbiType;
}

pub trait FromBuntValue: ToBuntValue {
    fn from_abi(val: Self::AbiType) -> Self;
}

impl ToBuntValue for f64 {
    fn bunt_type() -> Type {
        Type::Number
    }

    type AbiType = f64;

    fn to_abi(&self) -> Self::AbiType {
        *self
    }
}

impl FromBuntValue for f64 {
    fn from_abi(val: Self::AbiType) -> Self {
        val
    }
}
