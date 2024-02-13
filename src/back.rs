use std::ops::Deref;

use cranelift::{
    codegen::{
        ir::types::{F64, I64},
        Context,
    },
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use smallvec::{smallvec, SmallVec};

use crate::middle::{BinOp, ExprHandle, ExprKind, Function, Stmt, Type};
use cranelift::codegen::ir::Type as CType;

// ============= START TYPES =============

pub struct Backend {
    ctx: Context,
    module: JITModule,
    builder_ctx: FunctionBuilderContext,
}

struct FunctionCompiler<'f, 'b> {
    func: &'f Function,
    builder: FunctionBuilder<'b>,
    /// A variable may refer to multiple clif values
    vars: Vec<ShortVec<Variable>>,
}

/// A smallvec with some utility methods attached.
/// This is (exclusively?) used to map interpreter values/vars/types to
/// 0, 1, or multiple clif equivalents
#[derive(Debug)]
struct ShortVec<T>(SmallVec<[T; 4]>);

// ============= END TYPES =============

impl Backend {
    pub fn new() -> Self {
        let mut flag_builder: settings::Builder = settings::builder();
        flag_builder.set("opt_level", "speed").unwrap();
        //flag_builder.set("use_colocated_libcalls", "false").unwrap();
        //flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });

        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        let ctx = module.make_context();

        Backend {
            ctx,
            module,
            builder_ctx: FunctionBuilderContext::new(),
        }
    }

    pub fn compile(&mut self, func: &Function) -> *const u8 {
        self.module.clear_context(&mut self.ctx);

        let func_id = self
            .module
            .declare_function("__function__", Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        let mut compiler = FunctionCompiler {
            func,
            builder: FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx),
            vars: vec![],
        };
        compiler.compile();
        compiler.builder.finalize();

        self.module.define_function(func_id, &mut self.ctx).unwrap();

        self.module.finalize_definitions().unwrap();

        self.module.get_finalized_function(func_id)
    }
}

impl<'f, 'b> FunctionCompiler<'f, 'b> {
    pub fn compile(&mut self) {
        // build signature
        {
            let in_sig = self.func.sig.get().unwrap();

            let sig = &mut self.builder.func.signature;
            for arg_ty in in_sig.args.iter() {
                for t in lower_type(*arg_ty).iter() {
                    sig.params.push(AbiParam::new(t));
                }
            }
            for t in lower_type(in_sig.result).iter() {
                sig.returns.push(AbiParam::new(t));
            }
        }

        // build vars
        {
            let mut next_index = 0;
            self.vars = self
                .func.vars.iter()
                .map(|var| {
                    lower_type(var.ty)
                        .iter()
                        .map(|ty| {
                            let var = Variable::new(next_index);
                            self.builder.declare_var(var, ty);
                            next_index += 1;
                            var
                        })
                        .collect()
                })
                .collect();
        }

        // build entry block
        let entry_block = self.builder.create_block();
        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);
        self.builder.seal_block(entry_block);

        // build argument vars
        for index in 0..self.builder.func.signature.params.len() {
            let var = Variable::new(index);
            //let ty = self.builder.func.signature.params[index].value_type;
            //self.builder.declare_var(var, ty);

            let val = self.builder.block_params(entry_block)[index];
            self.builder.def_var(var, val);
        }

        let res = self.lower_expr(self.func.root);
        if let Some(res) = res {
            self.builder.ins().return_(&res);
        }
    }

    /// Returning `None` indicates a `never` value.
    pub fn lower_expr(&mut self, expr_h: ExprHandle) -> Option<ShortVec<Value>> {
        let expr = self.func.exprs.get(expr_h);
        match expr.kind {
            ExprKind::Number(val) => {
                // should match exactly
                assert!(expr.ty == Type::Number);

                Some(ShortVec::single(self.builder.ins().f64const(val)))
            }
            ExprKind::Local(var) => {
                // should match exactly (this may NOT be the case in the future with narrowing)
                assert!(expr.ty == self.func.vars.get(var).ty);

                let var = Variable::new(var.index());
                Some(ShortVec::single(self.builder.use_var(var)))
            }

            ExprKind::Binary(lhs, op, rhs) => {
                assert!(expr.ty.is(Type::Number));
                assert!(self.func.exprs.get(lhs).ty.is(Type::Number));
                assert!(self.func.exprs.get(rhs).ty.is(Type::Number));

                let lhs = self.lower_expr(lhs)?.expect_single();
                let rhs = self.lower_expr(rhs)?.expect_single();

                Some(ShortVec::single(match op {
                    BinOp::Add => self.builder.ins().fadd(lhs, rhs),
                    BinOp::Sub => self.builder.ins().fsub(lhs, rhs),
                    BinOp::Mul => self.builder.ins().fmul(lhs, rhs),
                    BinOp::Div => self.builder.ins().fdiv(lhs, rhs),
                    BinOp::Mod => {
                        panic!("cannot compile modulo, sorry :(");
                    }
                }))
            }
            ExprKind::Assign(lhs,rhs) => {
                assert!(expr.ty.is(Type::Void));

                let rhs_ty = self.func.exprs.get(rhs).ty;
                let rhs = self.lower_expr(rhs)?;

                self.lower_assign(lhs, rhs, rhs_ty)?;

                Some(ShortVec::empty())
            }
            
            /*ExprKind::Local(var) => {
                let var = Variable::new(var.index());
                Some(ShortVec::one(self.builder.use_var(var)))
            }*/

            ExprKind::Block { ref stmts, result } => {
                for s in stmts {
                    match s {
                        Stmt::Expr(e) => {
                            self.lower_expr(*e)?;
                        }
                        Stmt::Let { resolved_var, init, .. } => {

                            if let Some(init) = init {
                                let var = resolved_var.get().unwrap();
                                let var_ty = self.func.vars.get(*var).ty;

                                let init_ty = self.func.exprs.get(*init).ty;

                                assert!(init_ty.is(var_ty));

                                let clif_values = self.lower_expr(*init)?;
                                let clif_vars = &self.vars[var.index()];

                                assert!(clif_values.len() == clif_vars.len());
                                for (var,val) in clif_vars.iter().zip(clif_values.iter()) {
                                    self.builder.def_var(var, val);
                                }
                            }
                        }
                    }
                }

                if let Some(result) = result {
                    let res_val = self.lower_expr(result)?;
                    Some(res_val)
                } else {
                    Some(ShortVec::empty())
                }
            }

            ExprKind::If(c, t, Some(f)) => {
                let c = self.lower_expr(c)?.expect_single();

                let t_block = self.builder.create_block();
                let f_block = self.builder.create_block();
                let join_block = self.builder.create_block();

                for ty in lower_type(expr.ty).iter() {
                    self.builder.append_block_param(join_block, ty);
                }

                self.builder.ins().brif(c, t_block, &[], f_block, &[]);

                // true
                {
                    self.builder.switch_to_block(t_block);
                    self.builder.seal_block(t_block);

                    if let Some(vs) = self.lower_expr(t) {
                        self.builder.ins().jump(join_block, &vs);
                    }
                }

                // false
                {
                    self.builder.switch_to_block(f_block);
                    self.builder.seal_block(f_block);

                    if let Some(vs) = self.lower_expr(f) {
                        self.builder.ins().jump(join_block, &vs);
                    }
                }

                self.builder.switch_to_block(join_block);
                self.builder.seal_block(join_block);

                if !expr.ty.is_never() {
                    let rs = self.builder.block_params(join_block);
                    Some(ShortVec::new(rs))
                } else {
                    None
                }
            }
            ExprKind::Return(res) => {
                if let Some(res) = res {
                    let res = self.lower_expr(res)?;
                    self.builder.ins().return_(&res);
                } else {
                    self.builder.ins().return_(&[]);
                }
                None
            }
            ref e => panic!("TODO LOWER {:?}", e),
        }
    }

    fn lower_assign(&mut self, l_val: ExprHandle, r_val: ShortVec<Value>, r_ty: Type) -> Option<()> {
        let l_expr = self.func.exprs.get(l_val);
        assert!(l_expr.ty.is(r_ty));

        match l_expr.kind {
            ExprKind::Local(var) => {
                let clif_vars = &self.vars[var.index()];

                assert!(r_val.len() == clif_vars.len());
                for (var,val) in clif_vars.iter().zip(r_val.iter()) {
                    self.builder.def_var(var, val);
                }
            }
            ref e => panic!("TODO ASSIGN TO  {:?}", e)
        }

        Some(())
    }
}

impl<T> ShortVec<T>
where
    T: Copy,
{
    pub fn new(values: &[T]) -> Self {
        Self(SmallVec::from_slice(values))
    }

    pub fn single(val: T) -> Self {
        Self(smallvec!(val))
    }

    pub fn empty() -> Self {
        Self(SmallVec::new())
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = T> + 'a {
        self.0.iter().copied()
    }

    pub fn expect_single(self) -> T {
        assert!(self.0.len() == 1);
        self.0[0]
    }
}

impl<T> Deref for ShortVec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> FromIterator<T> for ShortVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(SmallVec::from_iter(iter))
    }
}

fn lower_type(ty: Type) -> ShortVec<CType> {
    match ty {
        Type::Number => ShortVec::single(F64),
        Type::Bool => ShortVec::single(I64),
        Type::Never => ShortVec::empty(),
        _ => panic!("can't convert type: {:?}", ty),
    }
}
