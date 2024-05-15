/*use std::collections::HashMap;

use crate::{
    errors::CompileErrorKind,
    ir::{Function, VarHandle},
};

#[derive(Default)]
pub struct ScopeStack<'vm> {
    scopes: Vec<HashMap<String, ScopeValue<'vm>>>,
}

pub enum ScopeValue<'vm> {
    Local(VarHandle<'vm>),
    Function(Function<'vm>),
}

impl<'vm> ScopeStack<'vm> {
    pub fn open(&mut self) {
        self.scopes.push(Default::default());
    }

    pub fn close(&mut self) -> HashMap<String, ScopeValue<'vm>> {
        self.scopes.pop().unwrap()
    }

    pub fn declare(&mut self, name: String, value: ScopeValue<'vm>) {
        let top = self.scopes.last_mut().expect("no top scope");
        top.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<&ScopeValue<'vm>, CompileErrorKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.get(name) {
                return Ok(res);
            }
        }
        Err(CompileErrorKind::CanNotResolve(name.to_owned()))
    }
}
*/
