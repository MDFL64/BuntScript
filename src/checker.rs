use std::collections::HashMap;

use crate::{handle_vec::HandleVec, middle::{ExprHandle, ExprKind, Function, OpKind, Stmt, Symbol, Type, Var, VarHandle}, types::Sig};

/// Single-pass checker which performs the following:
/// - Resolves symbol expressions.
/// - Infers and checks types.
pub struct Checker<'a> {
    func: &'a Function,
    scopes: Vec<Scope>,
    vars: HandleVec<Var>,
    adjustments: Vec<Adjust>,
    ret_ty: Type
}

#[derive(Debug)]
pub struct CheckError {

}

#[derive(Default)]
struct Scope {
    locals: HashMap<Symbol, VarHandle>
}

#[derive(Debug)]
enum Adjust {
    SetType(ExprHandle, Type),
    SetLocal(ExprHandle, VarHandle)
}

impl<'a> Checker<'a> {

    pub fn check(func: &mut Function) -> Result<(),CheckError> {
        assert!(func.vars.len() == 0);

        println!("!!! PRE");
        func.dump();

        let mut checker = Checker {
            func,
            scopes: vec!(Default::default()),
            vars: Default::default(),
            ret_ty: Type::Unknown,
            adjustments: vec!()
        };

        for (name,ty) in func.syn_args.iter() {
            checker.bind(&name, *ty);
        }

        // fill signature
        let args = checker.vars.iter().take(func.syn_args.len()).map(|var| var.ty).collect();
        {
            let res_ty = checker.check_expr(func.root)?;
            checker.ret_ty = checker.ret_ty.sum(res_ty)?;
        }
        let sig = Sig{
            args,
            result: checker.ret_ty
        };

        let adjustments = checker.adjustments;
        func.sig.set(sig).unwrap();
        func.vars = checker.vars;

        for adj in adjustments {
            match adj {
                Adjust::SetType(expr,ty) => {
                    func.exprs.get_mut(expr).ty = ty;
                }
                Adjust::SetLocal(expr, var) => {
                    func.exprs.get_mut(expr).kind = ExprKind::Local(var);
                }
            }
        }

        println!("!!! POST");
        func.dump();

        Ok(())
    }

    fn check_expr(&mut self, expr: ExprHandle) -> Result<Type,CheckError> {
        // TODO attach error context if missing
        let new_ty = self.check_expr_internal(expr)?;
        assert!(new_ty.is_known());

        self.adjustments.push(Adjust::SetType(expr, new_ty));
        Ok(new_ty)
    }

    fn check_expr_internal(&mut self, expr_h: ExprHandle) -> Result<Type,CheckError> {
        let expr = self.func.exprs.get(expr_h);

        match &expr.kind {
            ExprKind::Ident(name) => {
                let var = self.resolve(name)?;
                let ty = self.vars.get(var).ty;
                self.adjustments.push(Adjust::SetLocal(expr_h, var));
                Ok(ty)
            }
            ExprKind::Number(_) => {
                Ok(Type::Number)
            }
            ExprKind::Binary(l,op,r) => {
                let l = self.check_expr(*l)?;
                let r = self.check_expr(*r)?;

                if l.is_never() || r.is_never() {
                    Ok(Type::Never)
                } else if l.is_number() && r.is_number() {
                    Ok(match op.kind() {
                        OpKind::Arithmetic => Type::Number,
                        OpKind::Ordinal => Type::Bool,
                        OpKind::Equality => Type::Bool,
                    })
                } else {
                    Err(CheckError{ })
                }
            }
            ExprKind::Assign(l,r) => {
                let l = self.check_expr(*l)?;
                let r = self.check_expr(*r)?;
                l.unify(r)?; // types must be compatible
                
                if l.is_never() || r.is_never() {
                    Ok(Type::Never)
                } else {
                    Ok(Type::Void)
                }
            }

            ExprKind::Block { stmts, result } => {

                self.enter_block();

                let mut is_never = false;

                for s in stmts {
                    match s {
                        Stmt::Expr(e) => {
                            let e = self.check_expr(*e)?;
                            if e.is_never() {
                                is_never = true;
                            }
                        }
                        Stmt::Let { name, syn_ty, init, resolved_var } => {
                            let var = self.bind(name, syn_ty.unwrap_or(Type::Unknown));
                            resolved_var.set(var).unwrap();

                            if let Some(init) = init {
                                let init_ty = self.check_expr(*init)?;
                                let var = self.vars.get_mut(var);
                                var.ty = var.ty.unify(init_ty)?;

                                if init_ty.is_never() {
                                    is_never = true;
                                }
                            }

                            if !self.vars.get(var).ty.is_known() {
                                return Err(CheckError{});
                            }
                        }
                    }
                }

                let res_ty = if let Some(result) = result {
                    self.check_expr(*result)?
                } else {
                    Type::Void
                };

                self.exit_block();

                if is_never {
                    Ok(Type::Never)
                } else {
                    Ok(res_ty)
                }
            }
            ExprKind::If(c,t,Some(f)) => {
                let c = self.check_expr(*c)?;
                c.unify(Type::Bool)?;

                let t = self.check_expr(*t)?;
                let f = self.check_expr(*f)?;

                let res_ty = t.sum(f)?;

                if c.is_never() {
                    Ok(Type::Never)
                } else {
                    Ok(res_ty)
                }
            }
            ExprKind::While(c,body) => {
                let c = self.check_expr(*c)?;
                c.unify(Type::Bool)?;

                self.check_expr(*body)?;

                if c.is_never() {
                    Ok(Type::Never)
                } else {
                    Ok(Type::Void)
                }
            }
            ExprKind::Return(val) => {
                let ret_ty = if let Some(val) = val {
                    self.check_expr(*val)?
                } else {
                    Type::Void
                };

                self.ret_ty = self.ret_ty.sum(ret_ty)?;

                Ok(Type::Never)
            }
            _ => panic!("TODO CHECK {:?}",expr.kind)
        }
    }

    fn enter_block(&mut self) {
        self.scopes.push(Default::default());
    }

    fn exit_block(&mut self) {
        self.scopes.pop();
    }

    fn bind(&mut self, name: &Symbol, ty: Type) -> VarHandle {
        // resolve type HERE if needed
        let ty = ty;

        let var = self.vars.alloc(Var { ty });

        let scope = self.scopes.last_mut().unwrap();

        scope.locals.insert(name.clone(), var);

        var
    }

    fn resolve(&self, name: &Symbol) -> Result<VarHandle,CheckError> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.locals.get(name) {
                return Ok(*var)
            }
        }
        Err(CheckError {  })
    }
}
