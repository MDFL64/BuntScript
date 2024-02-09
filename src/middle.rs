use crate::{name_resolver::NameResolver, type_checker::TypeChecker};

// ============= START TYPES =============

pub struct Function {
    pub args: Vec<(Symbol, Type)>,
    pub ret_ty: Type,
    pub root: ExprId,
    var_tys: Vec<Type>,
    exprs: Vec<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub struct ExprId(usize);

pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
    pub pos: u32,
    pub check_done: bool,
}

#[derive(Debug)]
pub enum ExprKind {
    Number(f64),
    Binary(ExprId, BinOp, ExprId),

    Local(VarId),

    Return(ExprId),

    If(ExprId, ExprId, Option<ExprId>),

    // only exists in front
    Ident(Symbol),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    Void,
    Unknown,
    Never,
}

#[derive(Debug, Clone, Copy)]
pub struct VarId(usize);

// ============= END TYPES =============

impl ExprId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

impl VarId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

impl Function {
    pub fn new(args: Vec<(Symbol, Type)>, ret_ty: Type, exprs: Vec<Expr>, root: ExprId) -> Self {
        Self {
            args,
            ret_ty,
            exprs,
            root,

            var_tys: vec![],
        }
    }

    pub fn check(&mut self) {
        self.dump();

        // Replace all symbolic names and types.
        NameResolver::process(self);

        self.dump();

        TypeChecker::check(self);

        self.dump();
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id.index()]
    }

    pub fn expr_mut(&mut self, id: ExprId) -> &mut Expr {
        &mut self.exprs[id.index()]
    }

    pub fn var_ty(&self, id: VarId) -> Type {
        self.var_tys[id.index()]
    }

    pub fn iter_vars<'a>(&'a self) -> impl Iterator<Item = Type> + 'a {
        self.var_tys.iter().copied()
    }

    pub fn alloc_var(&mut self, ty: Type) -> VarId {
        let var = VarId::new(self.var_tys.len());
        self.var_tys.push(ty);
        var
    }

    pub fn dump(&self) {
        println!("args: {:?}", self.args);
        println!("returns: {:?}", self.ret_ty);
        for (i, expr) in self.exprs.iter().enumerate() {
            println!("{:5} = {:?} :: {:?}", i, expr.kind, expr.ty);
        }
    }
}

impl Expr {
    pub fn new(kind: ExprKind, pos: u32) -> Self {
        Self {
            kind,
            ty: Type::Unknown,
            pos,
            check_done: false,
        }
    }
}

impl Symbol {
    pub fn new(val: &str) -> Self {
        Self(val.to_owned())
    }
}

impl Type {
    pub fn is_number(&self) -> bool {
        match self {
            Type::Number => true,
            _ => false,
        }
    }

    pub fn is_never(&self) -> bool {
        match self {
            Type::Never => true,
            _ => false,
        }
    }

    pub fn is_known(&self) -> bool {
        match self {
            Type::Unknown => false,
            _ => true,
        }
    }

    pub fn unify(self, other: Type) -> Type {
        if self == other {
            self.clone()
        } else {
            match (self, other) {
                (ty, Type::Unknown) => ty.clone(),
                (Type::Unknown, ty) => ty.clone(),

                (ty, Type::Never) => ty.clone(),
                (Type::Never, ty) => ty.clone(),

                _ => panic!("unify {:?} {:?}", self, other),
            }
        }
    }

    pub fn sum(self, other: Type) -> Type {
        if self == other {
            self.clone()
        } else {
            match (self, other) {
                (ty, Type::Unknown) => ty.clone(),
                (Type::Unknown, ty) => ty.clone(),

                (ty, Type::Never) => ty.clone(),
                (Type::Never, ty) => ty.clone(),

                _ => panic!("sum {:?} {:?}", self, other),
            }
        }
    }
}
