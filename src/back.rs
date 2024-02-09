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

use crate::middle::{BinOp, ExprId, ExprKind, Function, Type};
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
    vars: Vec<ShortList<Variable>>,
}

/// A smallvec with some utility methods attached.
/// This is (exclusively?) used to map interpreter values/vars/types to
/// 0, 1, or multiple clif equivalents
#[derive(Debug)]
struct ShortList<T>(SmallVec<[T; 4]>);

// ============= END TYPES =============

impl Backend {
    pub fn new() -> Self {
        let flag_builder: settings::Builder = settings::builder();
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
            let sig = &mut self.builder.func.signature;
            for (_, arg) in self.func.args.iter() {
                for t in lower_type(*arg).iter() {
                    sig.params.push(AbiParam::new(t));
                }
            }
            for t in lower_type(self.func.ret_ty).iter() {
                sig.returns.push(AbiParam::new(t));
            }
        }

        // build vars
        {
            let mut next_index = 0;
            self.vars = self
                .func
                .iter_vars()
                .map(|var_ty| {
                    lower_type(var_ty)
                        .iter()
                        .map(|_| {
                            let var = Variable::new(next_index);
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
            let ty = self.builder.func.signature.params[index].value_type;
            self.builder.declare_var(var, ty);

            let val = self.builder.block_params(entry_block)[index];
            self.builder.def_var(var, val);
        }

        let res = self.lower_expr(self.func.root);
        if let Some(res) = res {
            self.builder.ins().return_(&res);
        }
    }

    pub fn lower_expr(&mut self, id: ExprId) -> Option<ShortList<Value>> {
        let expr = self.func.expr(id);
        match expr.kind {
            ExprKind::Return(res) => {
                let res = self.lower_expr(res)?;
                self.builder.ins().return_(&res);
                None
            }
            ExprKind::Binary(lhs, op, rhs) => {
                let lhs = self.lower_expr(lhs)?.expect_one();
                let rhs = self.lower_expr(rhs)?.expect_one();

                Some(ShortList::one(match op {
                    BinOp::Add => self.builder.ins().fadd(lhs, rhs),
                    BinOp::Sub => self.builder.ins().fsub(lhs, rhs),
                    BinOp::Mul => self.builder.ins().fmul(lhs, rhs),
                    BinOp::Div => self.builder.ins().fdiv(lhs, rhs),
                    BinOp::Mod => {
                        panic!("cannot compile modulo, sorry :(");
                    }
                }))
            }
            ExprKind::Number(val) => Some(ShortList::one(self.builder.ins().f64const(val))),
            ExprKind::Local(var) => {
                let var = Variable::new(var.index());
                Some(ShortList::one(self.builder.use_var(var)))
            }
            ExprKind::If(c, t, Some(f)) => {
                let c = self.lower_expr(c)?.expect_one();

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
                    Some(ShortList::new(rs))
                } else {
                    None
                }
            }
            ref e => panic!("TODO LOWER {:?}", e),
        }
    }
}

impl<T> ShortList<T>
where
    T: Copy,
{
    pub fn new(values: &[T]) -> Self {
        Self(SmallVec::from_slice(values))
    }

    pub fn one(val: T) -> Self {
        Self(smallvec!(val))
    }

    pub fn zero() -> Self {
        Self(SmallVec::new())
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = T> + 'a {
        self.0.iter().copied()
    }

    pub fn expect_one(self) -> T {
        assert!(self.0.len() == 1);
        self.0[0]
    }
}

impl<T> Deref for ShortList<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> FromIterator<T> for ShortList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(SmallVec::from_iter(iter))
    }
}

fn lower_type(ty: Type) -> ShortList<CType> {
    match ty {
        Type::Number => ShortList::one(F64),
        Type::Bool => ShortList::one(I64),
        Type::Never => ShortList::zero(),
        _ => panic!("can't convert type: {:?}", ty),
    }
}
