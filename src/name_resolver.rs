use std::collections::HashMap;

use crate::middle::{Symbol, Type, VarId};

#[derive(Debug)]
pub struct NameResolver {
    scopes: Vec<HashMap<Symbol, VarId>>,
    vars: Vec<Type>,
}

impl NameResolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            vars: vec![],
        }
    }

    pub fn add(&mut self, name: &Symbol, ty: &mut Type) -> VarId {
        // TODO resolve type???

        let scope = self.scopes.last_mut().unwrap();

        let id = VarId::new(self.vars.len());
        self.vars.push(ty.clone());

        scope.insert(name.clone(), id);

        id
    }

    pub fn get(&self, name: &Symbol) -> (VarId, Type) {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                let ty = &self.vars[var.index()];
                return (*var, ty.clone());
            }
        }
        panic!("name was not resolved: {:?}", name);
    }

    pub fn into_var_types(self) -> Vec<Type> {
        self.vars
    }
}
