use std::path::{Path, PathBuf};

use lalrpop_util::lalrpop_mod;

use crate::{handle_vec::{Handle, HandleVec}, middle::{Expr, ExprKind, Module, Type}};

// ============= START TYPES =============

pub struct ParserState {
    exprs: HandleVec<Expr>
}

// ============= END TYPES =============

#[derive(Debug)]
pub enum CompileError {
    FileNotFound(PathBuf),
    ParseError(String),
    ResolutionFailure,
    TypeError // TODO
}

lalrpop_util::lalrpop_mod!(syntax);

pub fn parse_module(path: impl AsRef<Path>) -> Result<Module, CompileError> {
    let source = std::fs::read_to_string(&path)
        .map_err(|_| CompileError::FileNotFound(path.as_ref().to_owned()))?;

    let mut state = ParserState::new();

    let res = syntax::ModuleParser::new().parse(&mut state, &source);

    res.map_err(|err| {
        // todo better errors
        CompileError::ParseError(format!("{:?}", err))
    })
}

impl ParserState {
    pub fn new() -> Self {
        Self {
            exprs: Default::default()
        }
    }

    pub fn alloc_expr(&mut self, kind: ExprKind) -> Handle<Expr> {
        self.exprs.alloc(Expr{
            kind,
            pos: 0,
            ty: Type::Error
        })
    }
}
