use std::{collections::VecDeque, ops::Deref};

use cranelift::{
    codegen::{
        ir::{
            condcodes::CondCode,
            types::{F64, I64, I8},
        },
        Context,
    },
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use smallvec::{smallvec, SmallVec};
use types::I32;

use crate::{
    errors::{CompileError, CompileErrorKind},
    front::{
        BinOp, Block, ExprHandle, ExprKind, FrontEnd, Function, FunctionBody, Sig, Stmt, Type,
        TypeKind,
    },
    util::get_or_try_init,
};

use cranelift::codegen::ir::Type as CType;

const PTR_TY: CType = I64;

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
    vars: Vec<ShortVec<Variable>>,
}

/// A smallvec with some utility methods attached.
/// This is (exclusively?) used to map bunt values/vars/types to
/// 0, 1, or multiple clif equivalents
#[derive(Debug, Clone)]
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
        compiler.compile()?;
        compiler.builder.finalize();

        self.module
            .define_function(*func_id, &mut self.ctx)
            .map_err(|err| CompileError {
                kind: CompileErrorKind::BackendError,
                message: err.to_string(),
            })?;

        Ok(())
    }

    /*pub fn finalize(&mut self) {
        self.module.finalize_definitions().unwrap();
    }*/

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

/// Convenience function for lowering.
fn res_value(val: Value) -> Result<Option<ShortVec<Value>>, CompileError> {
    Ok(Some(ShortVec::single(val)))
}

/// Convenience function for lowering.
fn res_void() -> Result<Option<ShortVec<Value>>, CompileError> {
    Ok(Some(ShortVec::empty()))
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
                let tys = lower_ty(var.ty);

                let tys: Vec<_> = tys
                    .iter()
                    .map(|ty| {
                        let var = Variable::new(next_index);
                        next_index += 1;
                        self.builder.declare_var(var, ty);
                        var
                    })
                    .collect();

                ShortVec::new(&tys)
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
            // TODO compound params
            let var = self.vars[index].expect_single();

            let val = self.builder.block_params(entry_block)[index];
            self.builder.def_var(var, val);
        }

        // lower body
        let res = self.lower_block(&self.func_body.block)?;

        if let Some(res) = res {
            // TODO non-trivial rets
            self.builder.ins().return_(&[res.expect_single()]);
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

    fn assign(&mut self, vars: &ShortVec<Variable>, values: &ShortVec<Value>) {
        assert!(vars.len() == values.len());
        for (var, val) in vars.iter().zip(values.iter()) {
            self.builder.def_var(var, val);
        }
    }

    fn lower_block(&mut self, block: &Block<'f>) -> Result<Option<ShortVec<Value>>, CompileError> {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Expr(expr_h) => {
                    let Some(_) = self.lower_expr(*expr_h)? else {
                        return Ok(None);
                    };
                }
                Stmt::Let(var_h, expr_h) => {
                    let Some(values) = self.lower_expr(*expr_h)? else {
                        return Ok(None);
                    };

                    let vars = self.vars[var_h.index()].clone();
                    self.assign(&vars, &values);
                }
            }
        }

        if let Some(result) = block.result {
            self.lower_expr(result)
        } else {
            Ok(Some(ShortVec::empty()))
        }
    }

    fn lower_place(&mut self, expr_h: ExprHandle<'f>) -> Result<ShortVec<Variable>, CompileError> {
        let expr = self.func_body.exprs.get(expr_h);
        let kind = &expr.kind;
        match kind {
            ExprKind::Var(var) => Ok(self.vars[var.index()].clone()),
            _ => Err(CompileError {
                kind: CompileErrorKind::CanNotResolve,
                message: format!("cannot assign to {:?}", kind),
            }),
        }
    }

    fn lower_expr(
        &mut self,
        expr_h: ExprHandle<'f>,
    ) -> Result<Option<ShortVec<Value>>, CompileError> {
        let expr = self.func_body.exprs.get(expr_h);
        let kind = &expr.kind;
        match kind {
            ExprKind::BinOp(lhs_h, op, rhs_h) => {
                if let BinOp::Assign = op {
                    let lhs = self.lower_place(*lhs_h)?;

                    let Some(rhs) = self.lower_expr(*rhs_h)? else {
                        return Ok(None);
                    };

                    self.assign(&lhs, &rhs);

                    res_void()
                } else {
                    let Some(lhs) = self.lower_expr(*lhs_h)? else {
                        return Ok(None);
                    };
                    let Some(rhs) = self.lower_expr(*rhs_h)? else {
                        return Ok(None);
                    };

                    let lhs = lhs.expect_single();
                    let rhs = rhs.expect_single();

                    match op {
                        BinOp::Add => res_value(self.builder.ins().fadd(lhs, rhs)),
                        BinOp::Sub => res_value(self.builder.ins().fsub(lhs, rhs)),
                        BinOp::Mul => res_value(self.builder.ins().fmul(lhs, rhs)),
                        BinOp::Div => res_value(self.builder.ins().fdiv(lhs, rhs)),

                        BinOp::Gt => {
                            res_value(self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs))
                        }
                        BinOp::Lt => {
                            res_value(self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs))
                        }

                        BinOp::Eq => {
                            let arg_ty = self.func_body.exprs.get(*lhs_h).ty;
                            match arg_ty.kind {
                                TypeKind::Number => {
                                    res_value(self.builder.ins().fcmp(FloatCC::Equal, lhs, rhs))
                                }
                                TypeKind::Bool => {
                                    res_value(self.builder.ins().icmp(IntCC::Equal, lhs, rhs))
                                }
                                _ => panic!("can not compare: {:?}", arg_ty),
                            }
                        }

                        _ => panic!("lower failed {:?}", op),
                    }
                }
            }
            ExprKind::Var(handle) => {
                let vars = &self.vars[handle.index()];
                let vars: Vec<_> = vars.iter().map(|var| self.builder.use_var(var)).collect();
                Ok(Some(ShortVec::new(&vars)))
            }
            ExprKind::Number(n) => {
                let val = self.builder.ins().f64const(*n);
                res_value(val)
            }
            ExprKind::Bool(b) => {
                let val = self.builder.ins().iconst(I8, *b as i64);
                res_value(val)
            }
            ExprKind::Block(block) => self.lower_block(block),
            ExprKind::If {
                cond,
                expr_then,
                expr_else,
            } => {
                let Some(cond) = self.lower_expr(*cond)? else {
                    return Ok(None);
                };
                let cond = cond.expect_single();

                if let Some(expr_else) = expr_else {
                    let bb_then = self.builder.create_block();
                    let bb_else = self.builder.create_block();
                    let bb_next = self.builder.create_block();

                    let res_ty = lower_ty(expr.ty);
                    for ty in res_ty.iter() {
                        self.builder.append_block_param(bb_next, ty);
                    }

                    self.builder.ins().brif(cond, bb_then, &[], bb_else, &[]);
                    self.builder.seal_block(bb_then);
                    self.builder.seal_block(bb_else);

                    {
                        self.builder.switch_to_block(bb_then);
                        if let Some(res) = self.lower_expr(*expr_then)? {
                            self.builder.ins().jump(bb_next, &res);
                        }
                    }
                    {
                        self.builder.switch_to_block(bb_else);
                        if let Some(res) = self.lower_expr(*expr_else)? {
                            self.builder.ins().jump(bb_next, &res);
                        }
                    }

                    self.builder.seal_block(bb_next);

                    self.builder.switch_to_block(bb_next);
                    let params = self.builder.block_params(bb_next);

                    Ok(Some(ShortVec::new(params)))
                } else {
                    let bb_then = self.builder.create_block();
                    let bb_next = self.builder.create_block();

                    self.builder.ins().brif(cond, bb_then, &[], bb_next, &[]);
                    self.builder.seal_block(bb_then);

                    {
                        self.builder.switch_to_block(bb_then);
                        if let Some(_) = self.lower_expr(*expr_then)? {
                            self.builder.ins().jump(bb_next, &[]);
                        }
                    }

                    self.builder.seal_block(bb_next);

                    self.builder.switch_to_block(bb_next);
                    Ok(Some(ShortVec::empty()))
                }
            }
            ExprKind::While { cond, body } => {
                let bb_cond = self.builder.create_block();
                let bb_body = self.builder.create_block();
                let bb_next = self.builder.create_block();

                self.builder.ins().jump(bb_cond,&[]);

                self.builder.switch_to_block(bb_cond);
                let Some(cond) = self.lower_expr(*cond)? else {
                    return Ok(None);
                };
                let cond = cond.expect_single();

                self.builder.ins().brif(cond, bb_body, &[], bb_next, &[]);
                self.builder.seal_block(bb_body);
                self.builder.seal_block(bb_next);

                {
                    self.builder.switch_to_block(bb_body);
                    if let Some(_) = self.lower_expr(*body)? {
                        self.builder.ins().jump(bb_cond, &[]);
                    }
                }

                self.builder.seal_block(bb_cond);
                self.builder.switch_to_block(bb_next);
                Ok(Some(ShortVec::empty()))
            }
            _ => panic!("lower expr {:?}", kind),
        }
    }

    /*
    /// Returning `None` indicates a `never` value.
    pub fn lower_expr(&mut self, expr: Expr<'vm>) -> ShortVec<Value> {
        match expr.kind {
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
            }/
            ref e => panic!("TODO LOWER {:?}", e),
        }
    }

    */
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

    pub fn as_single(&self) -> Option<T> {
        if self.0.len() == 1 {
            Some(self.0[0])
        } else {
            None
        }
    }

    pub fn expect_single(&self) -> T {
        self.as_single().expect("expected single element")
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

fn lower_ty_arg(ty: Type) -> CType {
    let ty_vec = lower_ty(ty);
    if let Some(ty) = ty_vec.as_single() {
        ty
    } else {
        PTR_TY
    }
}

fn lower_ty(ty: Type) -> ShortVec<CType> {
    match ty.kind {
        TypeKind::Number => ShortVec::single(F64),
        TypeKind::Bool => ShortVec::single(I8),
        TypeKind::Tuple(ref members) => {
            if members.len() == 0 {
                ShortVec::empty()
            } else {
                panic!("non-trivial tuple")
            }
        }
        _ => panic!("can't convert type: {:?}", ty),
    }
}

fn lower_sig(module: &JITModule, sig: &Sig) -> Signature {
    let mut clif_sig = module.make_signature();
    for ty in sig.args.iter() {
        let cty = lower_ty_arg(*ty);
        clif_sig.params.push(AbiParam::new(cty));
    }

    let rty = lower_ty_arg(sig.result);
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
