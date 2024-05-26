use crate::front::{FrontEnd, Type};

pub trait ConvertValue {
    fn bunt_type<'a>(front: &'a FrontEnd<'a>) -> Type<'a>;

    type AbiType;

    fn to_bunt(&self) -> Self::AbiType;

    fn from_bunt(val: Self::AbiType) -> Self;
}

// TODO: some or all of these traits should probably be marked unsafe
pub trait RetValue: ConvertValue {}
pub trait ArgValue: ConvertValue {}

impl ConvertValue for f64 {
    fn bunt_type<'a>(front: &'a FrontEnd<'a>) -> Type<'a> {
        front.common_types().number
    }

    type AbiType = f64;

    fn to_bunt(&self) -> Self::AbiType {
        *self
    }

    fn from_bunt(val: Self::AbiType) -> Self {
        val
    }
}

impl ArgValue for f64 {}
impl RetValue for f64 {}

impl ConvertValue for bool {
    fn bunt_type<'a>(front: &'a FrontEnd<'a>) -> Type<'a> {
        front.common_types().bool
    }

    type AbiType = u8;

    fn to_bunt(&self) -> Self::AbiType {
        *self as u8
    }

    fn from_bunt(val: Self::AbiType) -> Self {
        val != 0
    }
}

impl ArgValue for bool {}
impl RetValue for bool {}
