/*use typed_arena::Arena;

use super::ir::BinaryOp;
use crate::{
    errors::CompileError,
    types::{InternedType, Type},
};

/// Holds type variables and processes type constraints
pub struct TypeContext<'vm> {
    constraints: Vec<TypeConstraint<'vm>>,

    //pub invalid: Type<'vm>,
    //pub number: Type<'vm>
}

enum TypeConstraint<'vm> {
    OpBin {
        out: Type<'vm>,
        lhs: Type<'vm>,
        op: BinaryOp,
        rhs: Type<'vm>,
    },
}

impl<'vm> TypeContext<'vm> {
    pub fn new() -> Self {

        //let invalid: Type<'vm> = Type::new(types.alloc(InternedType::Invalid));
        //let number = Type::new(types.alloc(InternedType::Known(TypeKind::Number)));

        Self {
            constraints: Vec::new(),
        }
    }

    pub fn number(&self) -> Type<'vm> {
        panic!("num");
    }

    pub fn invalid(&self) -> Type<'vm> {
        panic!("invalid");
    }

    /*pub fn intern(&mut self, ty: TypeKind) -> Type {
        // TODO check against an interning table?
        // fast path for primitive types?
        let interned = self.types.alloc(InternedType::Known(ty));
        Type::new(interned)
    }*/

    pub fn add_op_bin(
        &mut self,
        lhs: Type<'vm>,
        op: BinaryOp,
        rhs: Type<'vm>,
    ) -> Result<Type<'vm>, CompileError> {
        let ty = self
            .solve_op_bin(lhs, op, rhs)?
            .unwrap_or_else(|| {
                panic!("todo add constraint");
                self.alloc_var()
            });

        Ok(ty)
    }

    fn solve_op_bin(
        &mut self,
        lhs: Type<'vm>,
        op: BinaryOp,
        rhs: Type<'vm>,
    ) -> Result<Option<Type<'vm>>, CompileError> {
        /*let Some(lhs) = self.resolve(lhs) else {
            return Ok(None);
        };
        let Some(rhs) = self.resolve(rhs) else {
            return Ok(None);
        };

        if lhs == Type::Number && rhs == Type::Number {
            Ok(Some(Type::Number))
        } else {
            panic!("solve bin op");
        }*/
        panic!("solve bin op");
    }
}
*/
