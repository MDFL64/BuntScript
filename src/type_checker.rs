use crate::middle::{ExprId, ExprKind, Function, Type};

pub struct TypeChecker<'a> {
    func: &'a mut Function,
    updates: u64
}

impl<'a> TypeChecker<'a> {

    pub fn check(func: &'a mut Function) {
        let mut checker = Self {
            func,
            updates: 0
        };

        let root_ty = checker.check_expr(checker.func.root);
        root_ty.unify(checker.func.ret_ty);

        let root_expr = checker.func.expr(checker.func.root);
        assert!(root_expr.check_done);
    }

    fn check_expr(&mut self, id: ExprId) -> Type {
        // fast path
        {
            let expr = self.func.expr(id);
            if expr.check_done {
                return expr.ty;
            }
        }

        {
            let new_ty = self.check_expr_internal(id);

            let expr = self.func.expr_mut(id);
            let res_ty = expr.ty.unify(new_ty);
            if expr.ty != res_ty {
                expr.ty = res_ty;
                self.updates += 1;
                self.update_done(id);
            }
            res_ty
        }
    }

    fn check_expr_internal(&mut self, id: ExprId) -> Type {
        let expr = self.func.expr(id);
        match expr.kind {
            ExprKind::If(c,t,Some(f)) => {
                self.check_expr(c).unify(Type::Bool);
                let t = self.check_expr(t);
                let f = self.check_expr(f);

                t.sum(f)
            }
            ExprKind::Binary(lhs,_op,rhs) => {
                let lhs = self.check_expr(lhs);
                let rhs = self.check_expr(rhs);

                if lhs.is_number() && rhs.is_number() {
                    Type::Number
                } else {
                    Type::Unknown
                }
            }
            ExprKind::Local(var) => {
                self.func.var_ty(var)
            }
            ExprKind::Number(_) => Type::Number,
            ExprKind::Return(e) => {
                self.check_expr(e).unify(self.func.ret_ty);
                Type::Never
            }
            ref e => panic!("TODO CHECK {:?}",e)
        }
    }

    fn update_done(&mut self, id: ExprId) {
        let expr = self.func.expr(id);

        // return early to prevent setting expr.check_done
        if !expr.ty.is_known() {
            return;
        }

        // check if children are done
        match expr.kind {
            ExprKind::Local(_) | ExprKind::Number(_) => (),
            ExprKind::Binary(a,_,b) => {
                if !self.func.expr(a).check_done {
                    return;
                }
                if !self.func.expr(b).check_done {
                    return;
                }
            }
            ExprKind::If(c,t,f) => {
                if !self.func.expr(c).check_done {
                    return;
                }
                if !self.func.expr(t).check_done {
                    return;
                }
                if let Some(f) = f {
                    if !self.func.expr(f).check_done {
                        return;
                    }
                }
            }
            ExprKind::Return(e) => {
                if !self.func.expr(e).check_done {
                    return;
                }
            }

            ref e => panic!("TODO UPDATE DONE {:?}",e)
        }

        self.func.expr_mut(id).check_done = true;
    }
}
