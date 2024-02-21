use crate::middle::Type;



pub trait ToBuntType {
    fn bunt_ty() -> Type;
}


impl ToBuntType for f64 {
    fn bunt_ty() -> Type {
        Type::Number
    }
}
