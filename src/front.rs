use std::path::{Path, PathBuf};

use lalrpop_util::lalrpop_mod;

use crate::middle::{Expr, ExprId, ExprKind, Function};

// ============= START TYPES =============

pub struct ParserState {
    exprs: Vec<Expr>,
}

// ============= END TYPES =============

#[derive(Debug)]
pub enum CompileError {
    FileNotFound(PathBuf),
    Parse(String),
}

lalrpop_util::lalrpop_mod!(syntax);

pub fn load_script(path: impl AsRef<Path>) -> Result<Function, CompileError> {
    let source = std::fs::read_to_string(&path)
        .map_err(|_| CompileError::FileNotFound(path.as_ref().to_owned()))?;

    let mut state = ParserState::new();

    let res = syntax::FunctionParser::new().parse(&mut state, &source);

    res.map_err(|err| {
        // todo better errors
        CompileError::Parse(format!("{:?}", err))
    })
}

impl ParserState {
    pub fn new() -> Self {
        Self { exprs: vec![] }
    }

    pub fn alloc_expr(&mut self, kind: ExprKind) -> ExprId {
        let id = ExprId::new(self.exprs.len());

        self.exprs.push(Expr::new(kind, 0));

        id
    }

    pub fn take_exprs(&mut self) -> Vec<Expr> {
        std::mem::take(&mut self.exprs)
    }
}
