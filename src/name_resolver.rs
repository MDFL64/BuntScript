use std::collections::HashMap;

use crate::middle::{ExprId, ExprKind, Function, Symbol, Type, VarId};

pub struct NameResolver<'a> {
    func: &'a mut Function,
    scopes: Vec<HashMap<Symbol, VarId>>,
}

impl<'a> NameResolver<'a> {

    pub fn process(func: &'a mut Function) {
        let mut resolver = Self {
            scopes: vec![HashMap::new()],
            func
        };

        let mut args = std::mem::take(&mut resolver.func.args);
        for (name, ty) in args.iter_mut() {
            let (_,new_ty) = resolver.bind(name, ty.clone());
            *ty = new_ty;
        }
        resolver.func.args = args;

        resolver.process_expr(resolver.func.root);
    }

    fn process_expr(&mut self, expr_id: ExprId) {
        let expr = self.func.expr(expr_id);
        match expr.kind {
            ExprKind::Number(_) => (),
            ExprKind::Binary(a, _, b) => {
                self.process_expr(a);
                self.process_expr(b);
            }
            ExprKind::Return(e) => self.process_expr(e),
            ExprKind::If(a, b, c) => {
                self.process_expr(a);
                self.process_expr(b);
                if let Some(c) = c {
                    self.process_expr(c);
                }
            }

            ExprKind::Ident(ref sym) => {
                let id = self.get(sym);
                self.func.expr_mut(expr_id).kind = ExprKind::Local(id);
            }

            ref e => panic!("TODO RESOLVE {:?}", e),
        }
    }

    fn bind(&mut self, name: &Symbol, ty: Type) -> (VarId,Type) {
        // resolve type HERE if needed
        let ty = ty;

        let var = self.func.alloc_var(ty.clone());

        let scope = self.scopes.last_mut().unwrap();

        scope.insert(name.clone(), var);

        (var,ty)
    }

    fn get(&self, name: &Symbol) -> VarId {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return *var
            }
        }
        panic!("name was not resolved: {:?}", name);
    }
}
