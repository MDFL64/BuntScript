use std::path::PathBuf;

use logos::Span;

#[derive(Debug, PartialEq)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub source: CompileErrorSource,
}

#[derive(Debug, PartialEq)]
pub enum CompileErrorKind {
    FileNotFound(PathBuf),
    ParseError(String),
    CanNotResolve(String),

    NotYetImplemented(String), /*FileNotFound(PathBuf),
                               ResolutionFailure,
                               TypeError, // TODO*/
}

#[derive(Debug, PartialEq)]
pub enum CompileErrorSource {
    Rust, // todo can we add any more context here?
    Bunt { file: PathBuf, span: Span },
}
