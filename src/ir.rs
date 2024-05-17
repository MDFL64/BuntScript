use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    path::PathBuf,
};

use typed_arena::Arena;

use crate::{
    front::pre_parse::{ParseItem, PreParse},
    handle_vec::{Handle, HandleVec},
};

#[derive(Default)]
pub struct Program<'p> {
    module_table: RefCell<HashMap<PathBuf, Module<'p>>>,
    modules: Arena<InternedModule<'p>>,
    types: Arena<InternedType<'p>>,
    items: Arena<InternedItem<'p>>,
}

type Module<'p> = &'p InternedModule<'p>;

#[derive(Default)]
pub struct InternedModule<'p> {
    // todo arena-allocate items as well? not sure exactly how
    items: RefCell<HashMap<String, Item<'p>>>,
}

type Item<'p> = &'p InternedItem<'p>;

pub enum InternedItem<'p> {
    Function(Function<'p>),
}

pub struct Function<'p> {
    sig: RefCell<Option<Sig<'p>>>,
}

pub struct Sig<'p> {
    args: Vec<Type<'p>>,
    res: Type<'p>,
}

type Type<'p> = &'p InternedType<'p>;

struct InternedType<'p> {
    kind: TypeKind,
    generics: Vec<Type<'p>>,
}

enum TypeKind {
    Number,
}

impl<'p> Program<'p> {
    pub fn add_module(&'p self, path: PathBuf) -> Module<'p> {
        let module = self.modules.alloc(InternedModule::default());

        let old = self.module_table.borrow_mut().insert(path, module);
        assert!(old.is_none());

        module
    }
}

impl<'p> InternedModule<'p> {
    pub fn register_items(&self, program: &'p Program<'p>, pre_parse: &PreParse) {
        let mut items = self.items.borrow_mut();

        for (key, parse_item) in pre_parse.items.iter() {
            let item = match parse_item {
                ParseItem::Function { .. } => InternedItem::Function(Function {
                    sig: RefCell::new(None),
                }),
            };

            let item = program.items.alloc(item);
            let old = items.insert(key.clone(), item);
            assert!(old.is_none());
        }
    }
}
