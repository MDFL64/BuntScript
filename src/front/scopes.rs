use std::collections::HashMap;

use crate::errors::{CompileError, CompileErrorKind};

use super::{code::VarHandle, front::Module, Function};

pub struct ScopeStack<'a> {
    scopes: Vec<HashMap<&'a str, VarHandle<'a>>>,
    root_module: &'a Module<'a>,
}

pub enum ScopeItem<'a> {
    Var(VarHandle<'a>),
    Item(&'a Function<'a>),
}

impl<'a> ScopeStack<'a> {
    pub fn new(root_module: &'a Module<'a>) -> Self {
        Self {
            scopes: Vec::new(),
            root_module,
        }
    }

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

    pub fn get(&self, name: &str) -> Result<ScopeItem<'a>, CompileError> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.get(name) {
                return Ok(ScopeItem::Var(*res));
            }
        }
        let items = self.root_module.items()?;
        if let Some(item) = items.get(name) {
            return Ok(ScopeItem::Item(item));
        }
        let prelude = self.root_module.prelude.borrow();
        if let Some(item) = prelude.get(name) {
            return Ok(ScopeItem::Item(item));
        }

        Err(CompileError {
            kind: CompileErrorKind::CanNotResolve,
            message: format!("failed to resolve symbol '{}'", name),
        })
    }
}
