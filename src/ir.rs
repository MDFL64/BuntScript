use std::{cell::OnceCell, collections::HashMap};

use cranelift_module::FuncId;
use logos::Span;
use typed_arena::Arena;

use crate::{
    errors::CompileError, handle_vec::{Handle, HandleVec}, single_pass::{ScopeStack, ScopeValue}, types::{InternedType, Sig, Type}
};

/// A Bunt program without the public API or state information
pub struct ProgramInternal<'vm> {
    pub modules: HandleVec<Module<'vm>>,
    pub stmts: HandleVec<Stmt<'vm>>,
    pub exprs: HandleVec<Expr<'vm>>,

    types: Arena<InternedType>,

    pub active_function: Option<Function<'vm>>

    //compiler: ProgramCompiler
}

impl<'vm> ProgramInternal<'vm> {
    pub fn new() -> Self {
        Self {
            modules: Default::default(),
            stmts: Default::default(),
            exprs: Default::default(),

            types: Arena::new(),

            active_function: None
        }
    }

    pub fn alloc_type_var(&'vm self) -> Type<'vm> {
        let interned = self.types.alloc(InternedType::Variable);
        Type::new(interned)
    }

    pub fn alloc_stmt(&mut self, kind: StmtKind<'vm>, span: Span) -> StmtHandle<'vm> {
        self.stmts.alloc(Stmt { kind, span })
    }

    pub fn alloc_expr(&mut self, kind: ExprKind<'vm>, span: Span) -> Result<ExprHandle<'vm>, CompileError> {
        let ty = match kind {
            ExprKind::Local(var) => {
                let func = self.active_function.as_ref().unwrap();
                func.get_var(var).ty
            }
            ExprKind::LitNumber(_) => {
                //self.tcx.number()
                panic!("number");
            }
            ExprKind::BinaryOp(lhs, op, rhs) => {
                let lhs_ty = self.exprs.get(lhs).ty;
                let rhs_ty = self.exprs.get(rhs).ty;

                //self.tcx.add_op_bin(lhs_ty, op, rhs_ty)?
                panic!("bin op");
            }
        };

        Ok(self.exprs.alloc(Expr { kind, span, ty }))
    }
}

pub struct Module<'vm> {
    scope: HashMap<String, ScopeValue<'vm>>,
}

pub struct Function<'vm> {
    pub name: String,
    pub arg_count: usize,
    pub ret_ty: Option<Type<'vm>>,
    pub body: Block<'vm>,
    pub clif_id: OnceCell<FuncId>,
    vars: HandleVec<Var<'vm>>,
}

pub type VarHandle<'vm> = Handle<Var<'vm>>;

#[derive(Debug)]
pub struct Var<'vm> {
    pub ty: Type<'vm>,
    pub name: String,
}

impl<'vm> Function<'vm> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            arg_count: 0,
            ret_ty: None,
            body: Block { stmts: Vec::new() },
            vars: HandleVec::default(),
            clif_id: OnceCell::new()
        }
    }

    pub fn sig(&self) -> Sig {
        let mut args = Vec::with_capacity(self.arg_count);

        for var in self.vars.iter() {
            if args.len() >= self.arg_count {
                break;
            }
            args.push(var.ty);
        }

        Sig{
            args,
            result: self.ret_ty.unwrap()
        }
    }

    pub fn declare_var(
        &mut self,
        name: &str,
        ty: Type<'vm>,
        scopes: &mut ScopeStack<'vm>,
    ) -> VarHandle {
        let name = name.to_owned();

        let var = self.vars.alloc(Var {
            ty,
            name: name.clone(),
        });

        scopes.declare(name, ScopeValue::Local(var));

        var
    }

    pub fn get_var(&self, handle: VarHandle<'vm>) -> &Var<'vm> {
        self.vars.get(handle)
    }

    pub fn iter_vars(&self) -> impl Iterator<Item = &Var> {
        self.vars.iter()
    }
}

#[derive(Debug)]
pub struct Block<'vm> {
    pub stmts: Vec<StmtHandle<'vm>>,
}

pub type StmtHandle<'vm> = Handle<Stmt<'vm>>;
pub type ExprHandle<'vm> = Handle<Expr<'vm>>;

pub struct Stmt<'vm> {
    pub kind: StmtKind<'vm>,
    pub span: Span,
}

pub struct Expr<'vm> {
    pub kind: ExprKind<'vm>,
    pub span: Span,
    pub ty: Type<'vm>,
}

pub enum StmtKind<'vm> {
    Return(Option<ExprHandle<'vm>>),
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'vm> {
    LitNumber(f64),
    Local(VarHandle<'vm>),
    BinaryOp(ExprHandle<'vm>, BinaryOp, ExprHandle<'vm>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
}
