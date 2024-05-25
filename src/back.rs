use std::{collections::VecDeque, ops::Deref};

use cranelift::{
    codegen::{
        ir::{
            condcodes::CondCode,
            types::{F64, I64},
        },
        Context,
    },
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use smallvec::{smallvec, SmallVec};

use crate::{
    errors::{CompileError, CompileErrorKind},
    front::{
        BinOp, Block, ExprHandle, ExprKind, FrontEnd, Function, FunctionBody, Sig, Type, TypeKind,
    },
    util::get_or_try_init,
};

use cranelift::codegen::ir::Type as CType;

pub struct BackEnd {
    ctx: Context,
    module: JITModule,
    builder_ctx: FunctionBuilderContext,
}

struct FunctionCompiler<'f, 'b> {
    func: &'f Function<'f>,
    func_body: &'f FunctionBody<'f>,
    builder: FunctionBuilder<'b>,
    module: &'b JITModule,
    vars: Vec<Variable>,
}

/// A smallvec with some utility methods attached.
/// This is (exclusively?) used to map bunt values/vars/types to
/// 0, 1, or multiple clif equivalents
#[derive(Debug)]
struct ShortVec<T>(SmallVec<[T; 4]>);

impl BackEnd {
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

        let clif_module = JITModule::new(builder);
        let ctx = clif_module.make_context();

        BackEnd {
            ctx,
            module: clif_module,
            builder_ctx: FunctionBuilderContext::new(),
        }
    }

    // should only be called by the compile queue
    fn compile_func<'a>(&mut self, func: &'a Function<'a>) -> Result<(), CompileError> {
        self.module.clear_context(&mut self.ctx);

        let func_id = func.clif_id.get().unwrap();

        let mut compiler = FunctionCompiler {
            func,
            func_body: func.body()?,
            builder: FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx),
            module: &self.module,
            vars: vec![],
        };
        compiler.compile();
        compiler.builder.finalize();

        self.module
            .define_function(*func_id, &mut self.ctx)
            .map_err(|err| CompileError {
                kind: CompileErrorKind::BackendError,
                message: err.to_string(),
            })?;

        Ok(())
    }

    pub fn finalize(&mut self) {
        self.module.finalize_definitions().unwrap();
    }

    pub fn get_code<'a>(&mut self, func: &'a Function<'a>) -> Result<*const u8, CompileError> {
        let mut compile_queue = CompileQueue::default();

        let clif_id = compile_queue.get_func_id(&mut self.module, func)?;

        compile_queue.run(self)?;

        self.module
            .finalize_definitions()
            .map_err(|err| CompileError {
                kind: CompileErrorKind::BackendError,
                message: err.to_string(),
            })?;

        Ok(self.module.get_finalized_function(clif_id))
    }
}

impl<'f, 'b> FunctionCompiler<'f, 'b> {
    pub fn compile(&mut self) -> Result<(), CompileError> {
        // build signature
        let in_sig = self.func.sig()?;
        self.builder.func.signature = lower_sig(&self.module, &in_sig.ty_sig);

        // build vars
        let mut next_index = 0;
        self.vars = self
            .func_body
            .vars
            .iter()
            .map(|(i, var)| {
                let ty = lower_arg(var.ty);

                let var = Variable::new(next_index);
                self.builder.declare_var(var, ty);
                next_index += 1;
                var
            })
            .collect();

        // build entry block
        let entry_block = self.builder.create_block();
        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);
        self.builder.seal_block(entry_block);

        // build argument vars
        for index in 0..self.builder.func.signature.params.len() {
            let var = Variable::new(index);

            let val = self.builder.block_params(entry_block)[index];
            self.builder.def_var(var, val);
        }

        // lower body
        let res = self.lower_block(&self.func_body.block)?;

        if let Some(res) = res {
            self.builder.ins().return_(&[res]);
        }

        Ok(())

        //panic!("STOP!");

        // build vars
        /*{

        if self.lower_block(&self.func.body).is_ok() {
            assert!(in_sig.result.resolve() == Some(&TypeKind::Void))
        }
        /*let res = self.lower_expr(self.func.root);
        if let Some(res) = res {
            self.builder.ins().return_(&res);
        }*/
        //panic!("back");*/
    }

    fn lower_block(&mut self, block: &Block<'f>) -> Result<Option<Value>, CompileError> {
        if let Some(result) = block.result {
            self.lower_expr(result)
        } else {
            Ok(None)
        }
    }

    fn lower_expr(&mut self, expr_h: ExprHandle) -> Result<Option<Value>, CompileError> {
        let kind = &self.func_body.exprs.get(expr_h).kind;
        match kind {
            ExprKind::BinOp(lhs, op, rhs) => {
                let Some(lhs) = self.lower_expr(*lhs)? else {
                    return Ok(None);
                };
                let Some(rhs) = self.lower_expr(*rhs)? else {
                    return Ok(None);
                };

                match op {
                    BinOp::Add => Ok(Some(self.builder.ins().fadd(lhs, rhs))),
                    BinOp::Sub => Ok(Some(self.builder.ins().fsub(lhs, rhs))),
                    BinOp::Mul => Ok(Some(self.builder.ins().fmul(lhs, rhs))),
                    BinOp::Div => Ok(Some(self.builder.ins().fdiv(lhs, rhs))),
                }
            }
            ExprKind::Var(handle) => {
                let var = self.vars[handle.index()];
                Ok(Some(self.builder.use_var(var)))
            }
            ExprKind::Number(n) => {
                let val = self.builder.ins().f64const(*n);
                Ok(Some(val))
            }
            _ => panic!("lower expr {:?}", kind),
        }
    }

    /*

    pub fn lower_block(&mut self, block: &Block<'vm>) -> Result<(), ()> {
        for stmt in block.stmts.iter() {
            match stmt.kind {
                /*Stmt::Let {
                    resolved_var, init, ..
                } => {
                    if let Some(init) = init {
                        let var = resolved_var.get().unwrap();
                        let var_ty = self.func.vars.get(*var).ty;

                        let init_ty = self.func.exprs.get(*init).ty;

                        assert!(init_ty == var_ty);

                        let clif_values = self.lower_expr(*init);
                        let clif_vars = &self.vars[var.index()];

                        assert!(clif_values.len() == clif_vars.len());
                        for (var, val) in clif_vars.iter().zip(clif_values.iter()) {
                            self.builder.def_var(var, val);
                        }
                    }
                }
                Stmt::Assign(lhs, rhs) => {
                    let rhs_ty = self.func.exprs.get(*rhs).ty;
                    let rhs = self.lower_expr(*rhs);

                    self.lower_assign(*lhs, rhs, rhs_ty);
                }
                Stmt::While(c, body) => {
                    let cond_block = self.builder.create_block();
                    let body_block = self.builder.create_block();
                    let next_block = self.builder.create_block();

                    self.builder.ins().jump(cond_block, &[]);

                    {
                        self.builder.switch_to_block(cond_block);
                        let c = self.lower_expr(*c).expect_single();
                        self.builder.ins().brif(c, body_block, &[], next_block, &[]);

                        self.builder.seal_block(body_block);
                        self.builder.seal_block(next_block);
                    }

                    {
                        self.builder.switch_to_block(body_block);

                        if self.lower_block(body).is_ok() {
                            self.builder.ins().jump(cond_block, &[]);
                        }
                    }

                    self.builder.seal_block(cond_block);
                    self.builder.switch_to_block(next_block);
                }*/
                StmtKind::Return(res) => {
                    if let Some(res) = res {
                        let res = self.lower_expr(res);
                        self.builder.ins().return_(&res);
                    } else {
                        self.builder.ins().return_(&[]);
                    }
                    return Err(());
                }
                _ => panic!("LOWER STMT {:?}", stmt.kind),
            }
        }
        Ok(())
    }

    /// Returning `None` indicates a `never` value.
    pub fn lower_expr(&mut self, expr: Expr<'vm>) -> ShortVec<Value> {
        match expr.kind {
            ExprKind::LitNumber(val) => {
                // should match exactly
                assert!(expr.ty.resolve() == Some(&TypeKind::Number));

                ShortVec::single(self.builder.ins().f64const(val))
            }
            ExprKind::Local(var) => {
                // should match exactly (this may NOT be the case in the future with narrowing)
                assert!(expr.ty == self.func.get_var(var).ty.unwrap());

                let var = Variable::new(var.index());
                ShortVec::single(self.builder.use_var(var))
            }

            ExprKind::BinaryOp(lhs, op, rhs) => {
                let lty = lhs.ty;
                let rty = rhs.ty;

                let lhs = self.lower_expr(lhs).expect_single();
                let rhs = self.lower_expr(rhs).expect_single();

                match op.kind() {
                    OpKind::Arithmetic => {
                        assert!(expr.ty.resolve() == Some(&TypeKind::Number));
                        assert!(lty.resolve() == Some(&TypeKind::Number));
                        assert!(rty.resolve() == Some(&TypeKind::Number));

                        ShortVec::single(match op {
                            BinaryOp::Add => self.builder.ins().fadd(lhs, rhs),
                            BinaryOp::Sub => self.builder.ins().fsub(lhs, rhs),
                            BinaryOp::Mul => self.builder.ins().fmul(lhs, rhs),
                            BinaryOp::Div => self.builder.ins().fdiv(lhs, rhs),
                            BinaryOp::Mod => {
                                panic!("cannot compile modulo, sorry :(");
                            }
                            _ => panic!(),
                        })
                    }
                    OpKind::Ordinal => {
                        assert!(expr.ty.resolve() == Some(&TypeKind::Bool));
                        assert!(lty.resolve() == Some(&TypeKind::Number));
                        assert!(rty.resolve() == Some(&TypeKind::Number));

                        ShortVec::single(match op {
                            BinaryOp::Lt => self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                            _ => panic!(),
                        })
                    }
                    _ => panic!("bad op"),
                }
            }

            /*ExprKind::Local(var) => {
                let var = Variable::new(var.index());
                Some(ShortVec::one(self.builder.use_var(var)))
            }*/

            /*ExprKind::Block { ref stmts, result } => {
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
            ExprKind::While(c, body) => {
                let cond_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let next_block = self.builder.create_block();

                self.builder.ins().jump(cond_block,&[]);

                {
                    self.builder.switch_to_block(cond_block);
                    let c = self.lower_expr(c)?.expect_single();
                    self.builder.ins().brif(c, body_block, &[], next_block, &[]);

                    self.builder.seal_block(body_block);
                    self.builder.seal_block(next_block);
                }

                {
                    self.builder.switch_to_block(body_block);

                    if let Some(vs) = self.lower_expr(body) {
                        self.builder.ins().jump(cond_block, &vs);
                    }
                }

                self.builder.seal_block(cond_block);
                self.builder.switch_to_block(next_block);

                Some(ShortVec::empty())
            }
            ExprKind::Return(res) => {
                if let Some(res) = res {
                    let res = self.lower_expr(res)?;
                    self.builder.ins().return_(&res);
                } else {
                    self.builder.ins().return_(&[]);
                }
                None
            }*/
            ref e => panic!("TODO LOWER {:?}", e),
        }
    }

    */

    /*fn lower_assign(
        &mut self,
        l_val: ExprHandle,
        r_val: ShortVec<Value>,
        r_ty: Type,
    ) -> Option<()> {
        let l_expr = self.func.exprs.get(l_val);
        assert!(l_expr.ty == r_ty);

        match l_expr.kind {
            ExprKind::Local(var) => {
                let clif_vars = &self.vars[var.index()];

                assert!(r_val.len() == clif_vars.len());
                for (var, val) in clif_vars.iter().zip(r_val.iter()) {
                    self.builder.def_var(var, val);
                }
            }
            ref e => panic!("TODO ASSIGN TO  {:?}", e),
        }

        Some(())
    }*/
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

fn lower_arg(ty: Type) -> CType {
    match ty.kind {
        TypeKind::Number => F64,
        //TypeKind::Bool => ShortVec::single(I64),
        _ => panic!("can't convert type: {:?}", ty),
    }
}

fn lower_sig(module: &JITModule, sig: &Sig) -> Signature {
    let mut clif_sig = module.make_signature();
    for ty in sig.args.iter() {
        let cty = lower_arg(*ty);
        clif_sig.params.push(AbiParam::new(cty));
    }

    let rty = lower_arg(sig.result);
    clif_sig.returns.push(AbiParam::new(rty));

    clif_sig
}

#[derive(Default)]
struct CompileQueue<'a> {
    queue: VecDeque<&'a Function<'a>>,
}

impl<'a> CompileQueue<'a> {
    pub fn run(&mut self, back: &mut BackEnd) -> Result<(), CompileError> {
        while let Some(func) = self.queue.pop_front() {
            back.compile_func(func)?;
        }

        Ok(())
    }

    pub fn get_func_id(
        &mut self,
        module: &mut JITModule,
        func: &'a Function<'a>,
    ) -> Result<FuncId, CompileError> {
        get_or_try_init(&func.clif_id, || {
            self.queue.push_back(func);

            let full_name = func.full_path();

            let sig = func.sig()?;

            let clif_sig = lower_sig(module, &sig.ty_sig);

            let clif_id = module
                .declare_function(&full_name, Linkage::Export, &clif_sig)
                .map_err(|err| CompileError {
                    kind: CompileErrorKind::BackendError,
                    message: err.to_string(),
                })?;

            Ok(clif_id)
        })
        .copied()
    }
}
