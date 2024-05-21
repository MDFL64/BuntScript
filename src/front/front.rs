use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Mutex, OnceLock},
};

use typed_arena::Arena;

use crate::errors::{CompileError, CompileErrorKind};

use super::{
    items::{Function, ModuleItems},
    lexer::{lex, TokenInfo},
    parser::Parser,
};

pub struct FrontEnd<'a> {
    /// Must be an absolute path.
    source_root: PathBuf,
    module_table: RefCell<HashMap<PathBuf, &'a Module<'a>>>,

    arena_sources: Arena<SourceFile<'a>>,
    arena_modules: Arena<Module<'a>>,
    arena_functions: Arena<Function<'a>>,
}

struct SourceFile<'a> {
    path: PathBuf,
    front: &'a FrontEnd<'a>,
    lazy: OnceCell<SourceFileLazy>,
}

struct SourceFileLazy {
    source: String,
    tokens: Vec<TokenInfo>,
}

pub struct Module<'a> {
    source: &'a SourceFile<'a>,
    items: OnceCell<ModuleItems<'a>>,
}

static SOURCE_CACHE: OnceLock<Mutex<HashMap<PathBuf, String>>> = OnceLock::new();

impl<'a> FrontEnd<'a> {
    /// Constructs a new front-end. Panics if the provided path is not absolute.
    pub fn new(source_root: PathBuf) -> Self {
        assert!(source_root.is_absolute());

        Self {
            source_root,
            module_table: RefCell::new(HashMap::new()),

            arena_sources: Arena::new(),
            arena_modules: Arena::new(),
            arena_functions: Arena::new(),
        }
    }

    pub(super) fn alloc_function(&'a self, func: Function<'a>) -> &'a Function<'a> {
        self.arena_functions.alloc(func)
    }

    fn load_file(&self, path: &Path) -> Result<String, CompileError> {
        let mut full_path = self.source_root.clone();
        full_path.push(path);

        // source caching -- used for benchmarking
        // DO NOT ENABLE for normal use
        let cache = SOURCE_CACHE.get_or_init(|| Default::default());
        let mut cache = cache.lock().unwrap();

        if let Some(res) = cache.get(&full_path) {
            return Ok(res.clone());
        }

        let res = std::fs::read_to_string(&full_path).map_err(|_| CompileError {
            kind: CompileErrorKind::FileReadFailed,
            message: format!("failed to read file: {:?}", full_path),
        });

        if let Ok(ref res) = res {
            cache.insert(full_path, res.clone());
        }

        res
    }

    pub fn module(&'a self, path: &Path) -> &'a Module<'a> {
        let mut module_table = self.module_table.borrow_mut();

        if let Some(module) = module_table.get(path) {
            module
        } else {
            let path = path.to_owned();

            let source = self.arena_sources.alloc(SourceFile {
                path: path.clone(),
                front: self,
                lazy: OnceCell::new(),
            });

            let module = self.arena_modules.alloc(Module {
                source,
                items: OnceCell::new(),
            });

            let old = module_table.insert(path, module);
            assert!(old.is_none());

            module
        }
    }
}

impl<'a> Module<'a> {
    pub fn items(&self) -> Result<&ModuleItems<'a>, CompileError> {
        get_or_try_init(&self.items, || {
            let source = self.source.get()?;
            let mut parser = Parser::new(&source.source, &source.tokens, self.source.front);
            ModuleItems::parse(&mut parser)
        })
    }
}

impl<'a> SourceFile<'a> {
    pub fn get(&self) -> Result<&SourceFileLazy, CompileError> {
        get_or_try_init(&self.lazy, || {
            let source = self.front.load_file(&self.path)?;
            let tokens = lex(&source)?;

            Ok(SourceFileLazy { source, tokens })
        })
    }
}

// the std method is unstable
fn get_or_try_init<T, E>(cell: &OnceCell<T>, init: impl FnOnce() -> Result<T, E>) -> Result<&T, E> {
    if let Some(res) = cell.get() {
        Ok(res)
    } else {
        let old = cell.set(init()?);
        assert!(old.is_ok());
        Ok(cell.get().unwrap())
    }
}
