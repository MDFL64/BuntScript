use std::collections::HashMap;

use crate::errors::{CompileError, CompileErrorKind};

use super::code::VarHandle;

#[derive(Default)]
pub struct ScopeStack<'a> {
    scopes: Vec<HashMap<&'a str, VarHandle<'a>>>,
}

impl<'a> ScopeStack<'a> {
    pub fn open(&mut self) {
        self.scopes.push(Default::default());
    }

    pub fn close(&mut self) -> HashMap<&'a str, VarHandle<'a>> {
        self.scopes.pop().unwrap()
    }

    pub fn declare(&mut self, name: &'a str, value: VarHandle<'a>) {
        let top = self.scopes.last_mut().expect("no top scope");
        top.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<VarHandle<'a>, CompileError> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.get(name) {
                return Ok(*res);
            }
        }
        Err(CompileError {
            kind: CompileErrorKind::CanNotResolve,
            message: format!("failed to resolve symbol '{}'", name),
        })
    }
}
