use crate::{
    errors::CompileError,
    ir::BinaryOp,
    types::{Type, TypeKind},
};

pub struct Checker<'vm> {
    _a: Type<'vm>,
}

impl<'vm> Checker<'vm> {
    pub fn solve_op_bin(
        lhs_ty: Type<'vm>,
        op: BinaryOp,
        rhs_ty: Type<'vm>,
    ) -> Result<Option<Type<'vm>>, CompileError> {
        let Some(lhs) = lhs_ty.resolve() else {
            return Ok(None);
        };
        let Some(rhs) = rhs_ty.resolve() else {
            return Ok(None);
        };

        if lhs == &TypeKind::Number && rhs == &TypeKind::Number {
            Ok(Some(lhs_ty))
        } else {
            panic!("solve bin op");
        }
    }

    /// Add an assignment. This adjusts type variables and 
    /// does not need to be handled by the solver.
    pub fn assign(lhs_ty: Type<'vm>, rhs_ty: Type<'vm>) -> Result<(), CompileError> {
        println!("{:?} := {:?}",lhs_ty,rhs_ty);
        Ok(())
    }
}
