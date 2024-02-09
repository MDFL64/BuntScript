use crate::name_resolver::NameResolver;

// ============= START TYPES =============

pub struct Function {
    pub args: Vec<(Symbol, Type)>,
    pub ret_ty: Type,
    pub var_tys: Vec<Type>,
    pub root: ExprId,
    exprs: Vec<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub struct ExprId(usize);

pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
    pub pos: u32,
    checked: bool,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    Void,
    Unknown,
    Never,
}

#[derive(Debug, Clone, Copy)]
pub struct VarId(usize);

#[derive(Debug)]
enum TypeCheckStatus {
    Done,
    Working,
    Stuck,
}

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

        let mut resolver = NameResolver::new();

        for (name, ty) in self.args.iter_mut() {
            resolver.add(name, ty);
        }

        self.resolve_names(self.root, &mut resolver);

        self.var_tys = resolver.into_var_types();

        let res = self.check_types();
        if let TypeCheckStatus::Done = res {
            self.dump();
        } else {
            panic!("type check did not complete");
        }

        let root_ty = &self.expr(self.root).ty;
        self.ret_ty.unify(&root_ty);
    }

    fn resolve_names(&mut self, expr_id: ExprId, resolver: &mut NameResolver) {
        let expr = &mut self.exprs[expr_id.0];
        match expr.kind {
            ExprKind::Number(_) => (),
            ExprKind::Binary(a, _, b) => {
                self.resolve_names(a, resolver);
                self.resolve_names(b, resolver);
            }
            ExprKind::Return(e) => self.resolve_names(e, resolver),
            ExprKind::If(a, b, c) => {
                self.resolve_names(a, resolver);
                self.resolve_names(b, resolver);
                if let Some(c) = c {
                    self.resolve_names(c, resolver);
                }
            }

            ExprKind::Ident(ref sym) => {
                let (id, ty) = resolver.get(sym);
                expr.kind = ExprKind::Local(id);
                expr.ty = ty;
            }

            ref e => panic!("TODO RESOLVE {:?}", e),
        }
    }

    fn check_types(&mut self) -> TypeCheckStatus {
        let mut updated = 0;
        let mut bad = false;

        for i in 0..self.exprs.len() {
            let expr = &self.exprs[i];
            if expr.checked {
                continue;
            }
            let res = self.check_expr_type(expr);

            if let Some(new_ty) = res {
                let final_ty = new_ty.unify(&expr.ty);
                if expr.ty != final_ty {
                    updated += 1;
                }
                self.exprs[i].checked = final_ty.is_known();
                self.exprs[i].ty = final_ty;
            }

            if !self.exprs[i].checked {
                bad = true;
            }
        }

        if !bad {
            TypeCheckStatus::Done
        } else {
            if updated == 0 {
                TypeCheckStatus::Stuck
            } else {
                TypeCheckStatus::Working
            }
        }
    }

    fn check_expr_type(&self, expr: &Expr) -> Option<Type> {
        match expr.kind {
            ExprKind::Number(_) => Some(Type::Number),
            ExprKind::Local(var) => {
                let var_ty = self.var_ty(var);
                Some(var_ty)
            }
            ExprKind::Binary(lhs, _op, rhs) => {
                let lhs = &self.expr(lhs).ty;
                let rhs = &self.expr(rhs).ty;

                if lhs.is_known() && rhs.is_known() {
                    if lhs.is_number() && rhs.is_number() {
                        Some(Type::Number)
                    } else {
                        panic!("bad arithmetic");
                    }
                } else {
                    None
                }
            }
            ExprKind::If(c, t, Some(f)) => {
                let c = &self.expr(c).ty;
                c.unify(&Type::Bool);

                let t = &self.expr(t).ty;
                let f = &self.expr(f).ty;

                Some(t.sum(f))
            }
            ExprKind::Return(e) => {
                let e = &self.expr(e).ty;
                if e.is_known() {
                    e.unify(&self.ret_ty);
                    Some(Type::Never)
                } else {
                    None
                }
            }
            ref e => panic!("TODO CHECK {:?}", e),
        }
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id.index()]
    }

    fn var_ty(&self, id: VarId) -> Type {
        self.var_tys[id.index()].clone()
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
            checked: false,
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

    pub fn unify(&self, other: &Type) -> Type {
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

    pub fn sum(&self, other: &Type) -> Type {
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
