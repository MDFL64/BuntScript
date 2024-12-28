use std::{
    borrow::{Borrow, Cow}, cell::{OnceCell, RefCell}, collections::HashMap, path::{Path, PathBuf}, sync::{Arc, Mutex, OnceLock}
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
    pub text: Cow<'static, str>,
    pub tokens: Vec<TokenInfo>,
}

pub struct Module<'a> {
    source_file: &'a SourceFile<'a>,
    items: OnceCell<ModuleItems<'a>>,
    pub prelude: Arc<RefCell<ModuleItems<'a>>>
}

static SOURCE_CACHE: OnceLock<Mutex<HashMap<PathBuf, String>>> = OnceLock::new();

pub enum ModuleSource<'a> {
    File(&'a Path),
    Raw{
        name: &'a str,
        source: &'static str
    }
}

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

    pub fn module(&'a self, source: ModuleSource, prelude: &Arc<RefCell<ModuleItems<'a>>>) -> Result<&'a Module<'a>, CompileError> {
        let mut module_table = self.module_table.borrow_mut();

        let path = match source {
            ModuleSource::File(path) => Cow::Borrowed(path),
            ModuleSource::Raw { name, .. } => Cow::Owned(PathBuf::from(format!("::{name}")))
        };

        if let Some(module) = module_table.get(path.as_ref()) {
            Ok(module)
        } else {
            let path = path.to_owned();

            let lazy_source = match source {
                ModuleSource::File(path) => OnceCell::new(),
                ModuleSource::Raw { source, .. } => {
                    OnceCell::from(SourceFileLazy::new_raw(source)?)
                }
            };

            let source_file = self.arena_sources.alloc(SourceFile {
                path: path.clone().into_owned(),
                front: self,
                lazy: lazy_source
            });

            let module = self.arena_modules.alloc(Module {
                source_file,
                items: OnceCell::new(),
                prelude: prelude.clone()
            });

            let old = module_table.insert(path.into_owned(), module);
            assert!(old.is_none());

            Ok(module)
        }
    }
}

impl<'a> Module<'a> {
    pub fn items(&'a self) -> Result<&ModuleItems<'a>, CompileError> {
        get_or_try_init(&self.items, || {
            let source = self.source_file.get()?;
            let mut parser = Parser::new(self, &source.tokens)?;
            ModuleItems::parse(&mut parser)
        })
    }

    pub fn source_text(&self) -> Result<&'a str, CompileError> {
        Ok(&self.source_file.get()?.text)
    }

    pub fn source_path(&self) -> String {
        self.source_file.path.to_str().unwrap().to_owned()
    }

    pub fn front(&self) -> &'a FrontEnd<'a> {
        self.source_file.front
    }
}

impl<'a> SourceFile<'a> {
    pub fn get(&self) -> Result<&SourceFileLazy, CompileError> {
        get_or_try_init(&self.lazy, || {
            let text = Cow::Owned(self.front.load_file(&self.path)?);
            let tokens = lex(&text)?;

            Ok(SourceFileLazy { text, tokens })
        })
    }
}

impl SourceFileLazy {
    // todo? it would be really cool to lex and parse raw modules at host compile time
    fn new_raw(text: &'static str) -> Result<Self, CompileError> {
        let text = Cow::Borrowed(text);
        let tokens = lex(&text)?;
        Ok(SourceFileLazy{
            text,
            tokens
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
