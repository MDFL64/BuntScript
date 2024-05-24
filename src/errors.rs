#[derive(Debug)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub message: String,
    //pub source_file_name: String,
    //pub source_locs: Vec<SourceLoc>,
    //pub has_rust_source: bool,
}

#[derive(Debug, PartialEq)]
pub enum CompileErrorKind {
    /// The file either does not exist, could not be read, or resides outside our source root.
    FileReadFailed,
    /// Used for any errors in lexing or parsing.
    SyntaxError,
    DuplicateDeclarations,
    CanNotResolve,
    TypeError,
    BackendError,

    NotYetImplemented, /*FileNotFound(PathBuf),
                       ResolutionFailure,
                       TypeError, // TODO*/
}

/*#[derive(Debug)]
pub enum CompileErrorSource {
    Rust, // todo can we add any more context here?
    Bunt { file_name: String, loc: SourceLoc },
}*/
