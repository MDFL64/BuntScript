use std::{marker::PhantomData, path::PathBuf};

use logos::Logos;
use self_cell::self_cell;

use crate::{errors::{CompileError, CompileErrorKind}, front::{lexer::{lex, TokenInfo}, parser::Parser}};

use self::items::ModuleF;

mod lexer;
mod parser;

mod items;
mod types;

pub struct SourceFile {
    /// Relative to source root
    path: PathBuf,
    tree: SourceCell
}

self_cell! {
    pub struct SourceCell {
        owner: String,
        #[covariant]
        dependent: SourceTree,
    }
}

struct SourceTree<'a> {
    module: ModuleF<'a>,
    tokens: Vec<TokenInfo>
}

impl SourceFile {
    pub fn new(path: PathBuf, source: String) -> Result<Self, CompileError> {
        Ok(Self {
            path,
            tree: SourceCell::try_new(source, build_tree)?
        })
    }
}

fn build_tree<'a>(source: &'a String) -> Result<SourceTree<'a>, CompileError> {

    let tokens = lex(source)?;

    let mut parser = Parser::new(source, &tokens);

    let module = ModuleF::parse(&mut parser)?;

    Ok(SourceTree {
        module,
        tokens
    })
}

