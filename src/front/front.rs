use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Mutex, OnceLock},
};

use typed_arena::Arena;

use crate::{
    errors::{CompileError, CompileErrorKind},
    util::get_or_try_init,
};

use super::{
    items::{Function, ModuleItems},
    lexer::{lex, TokenInfo},
    parser::Parser,
    types::{Type, TypeKind},
};

pub struct FrontEnd<'a> {
    /// Must be an absolute path.
    source_root: PathBuf,
    module_table: RefCell<HashMap<PathBuf, &'a Module<'a>>>,

    arena_sources: Arena<SourceFile<'a>>,
    arena_modules: Arena<Module<'a>>,
    arena_functions: Arena<Function<'a>>,
}

pub struct SourceFile<'a> {
    pub path: PathBuf,
    pub front: &'a FrontEnd<'a>,
    lazy: OnceCell<SourceFileLazy>,
}

pub struct SourceFileLazy {
    pub text: String,
    pub tokens: Vec<TokenInfo>,
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

    fn load_file(&self, raw_path: &Path) -> Result<String, CompileError> {
        let path = normalize_path(raw_path).map_err(|_| CompileError {
            kind: CompileErrorKind::FileReadFailed,
            message: format!(
                "attempt to read file outside of source root: {:?}",
                raw_path
            ),
        })?;

        let path_str = path.as_os_str().to_str();
        if let Some(path_str) = path_str {
            if path_str.contains(':') {
                return Err(CompileError {
                    kind: CompileErrorKind::FileReadFailed,
                    message: format!("script paths may not contain ':': {:?}", raw_path),
                });
            }
        } else {
            return Err(CompileError {
                kind: CompileErrorKind::FileReadFailed,
                message: format!("script path is not a valid string: {:?}", raw_path),
            });
        }

        let mut full_path = self.source_root.clone();
        full_path.push(path);

        // source caching -- used for benchmarking
        // DO NOT ENABLE for normal use
        const USE_CACHE: bool = false;

        if USE_CACHE {
            let cache = SOURCE_CACHE.get_or_init(|| Default::default());
            let cache = cache.lock().unwrap();

            if let Some(res) = cache.get(&full_path) {
                return Ok(res.clone());
            }
        }

        let res = std::fs::read_to_string(&full_path).map_err(|_| CompileError {
            kind: CompileErrorKind::FileReadFailed,
            message: format!("failed to read file: {:?}", full_path),
        });

        if USE_CACHE {
            let cache = SOURCE_CACHE.get_or_init(|| Default::default());
            let mut cache = cache.lock().unwrap();

            if let Ok(ref res) = res {
                cache.insert(full_path, res.clone());
            }
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
    pub fn items(&'a self) -> Result<&ModuleItems<'a>, CompileError> {
        get_or_try_init(&self.items, || {
            let source = self.source.get()?;
            let mut parser = Parser::new(self, &source.tokens)?;
            ModuleItems::parse(&mut parser)
        })
    }

    pub fn source_text(&self) -> Result<&'a str, CompileError> {
        Ok(&self.source.get()?.text)
    }

    pub fn source_path(&self) -> String {
        self.source.path.to_str().unwrap().to_owned()
    }

    pub fn front(&self) -> &'a FrontEnd<'a> {
        self.source.front
    }
}

impl<'a> SourceFile<'a> {
    pub fn get(&self) -> Result<&SourceFileLazy, CompileError> {
        get_or_try_init(&self.lazy, || {
            let text = self.front.load_file(&self.path)?;
            let tokens = lex(&text)?;

            Ok(SourceFileLazy { text, tokens })
        })
    }
}

fn normalize_path(path: &Path) -> Result<PathBuf, ()> {
    let mut result = PathBuf::new();

    if path.is_absolute() {
        return Err(());
    }

    for item in path.iter() {
        if item == "." {
            continue;
        }
        if item == ".." {
            if !result.pop() {
                return Err(());
            }
        }
        result.push(item);
    }

    Ok(result)
}
