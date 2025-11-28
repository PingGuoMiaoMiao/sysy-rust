// AST -> IR 转换
use crate::ast;
use crate::ir::*;
use std::collections::HashMap;

pub struct IRGenerator {
    module: Module,
    current_function: Option<usize>,
    current_block: Option<String>,
    reg_counter: usize,
    label_counter: usize,
    symbol_table: Vec<HashMap<String, Value>>,
    loop_stack: Vec<LoopContext>,
}

struct LoopContext {
    continue_label: String,
    break_label: String,
}

pub type IRResult<T> = Result<T, String>;

impl IRGenerator {
    pub fn new() -> Self {
        IRGenerator {
            module: Module::new(),
            current_function: None,
            current_block: None,
            reg_counter: 0,
            label_counter: 0,
            symbol_table: vec![HashMap::new()],  // 全局作用域
            loop_stack: Vec::new(),
        }
    }

    pub fn generate(mut self, comp_unit: &ast::CompUnit) -> IRResult<Module> {
        self.gen_comp_unit(comp_unit)?;
        Ok(self.module)
    }

    // 辅助函数
    fn new_reg(&mut self, ty: Type) -> VirtualReg {
        let id = self.reg_counter;
        self.reg_counter += 1;
        VirtualReg::new(id, ty)
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let id = self.label_counter;
        self.label_counter += 1;
        format!("{}{}", prefix, id)
    }

    fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.symbol_table.pop();
    }

    fn lookup(&self, name: &str) -> Option<&Value> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }

    fn define(&mut self, name: String, value: Value) {
        self.symbol_table.last_mut().unwrap().insert(name, value);
    }

    fn current_func_mut(&mut self) -> &mut Function {
        let idx = self.current_function.expect("Not in a function");
        &mut self.module.functions[idx]
    }

    fn current_bb_mut(&mut self) -> &mut BasicBlock {
        let label = self.current_block.as_ref().expect("No current block").clone();
        self.current_func_mut()
            .find_block_mut(&label)
            .expect("Current block not found")
    }

    fn emit(&mut self, inst: Instruction) {
        self.current_bb_mut().instructions.push(inst);
    }

    fn emit_terminator(&mut self, term: Terminator) {
        self.current_bb_mut().terminator = Some(term);
    }

    fn create_block(&mut self, label: String) {
        let bb = BasicBlock::new(label);
        self.current_func_mut().basic_blocks.push(bb);
    }

    fn switch_to_block(&mut self, label: String) {
        self.current_block = Some(label);
    }

    fn convert_type(&self, btype: &ast::BType) -> Type {
        match btype {
            ast::BType::Int => Type::Int,
            ast::BType::Void => Type::Void,
        }
    }

    fn convert_binary_op(&self, op: &ast::BinaryOp) -> BinaryOp {
        match op {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
            ast::BinaryOp::Mod => BinaryOp::Mod,
            ast::BinaryOp::Lt => BinaryOp::Lt,
            ast::BinaryOp::Gt => BinaryOp::Gt,
            ast::BinaryOp::Le => BinaryOp::Le,
            ast::BinaryOp::Ge => BinaryOp::Ge,
            ast::BinaryOp::Eq => BinaryOp::Eq,
            ast::BinaryOp::Ne => BinaryOp::Ne,
            ast::BinaryOp::And => BinaryOp::And,
            ast::BinaryOp::Or => BinaryOp::Or,
        }
    }

    fn convert_unary_op(&self, op: &ast::UnaryOp) -> Option<UnaryOp> {
        match op {
            ast::UnaryOp::Neg => Some(UnaryOp::Neg),
            ast::UnaryOp::Not => Some(UnaryOp::Not),
            ast::UnaryOp::Pos => None,
        }
    }

    fn gen_comp_unit(&mut self, comp_unit: &ast::CompUnit) -> IRResult<()> {
        for item in &comp_unit.items {
            match item {
                ast::CompUnitItem::Decl(decl) => self.gen_global_decl(decl)?,
                ast::CompUnitItem::FuncDef(func_def) => self.gen_func_def(func_def)?,
            }
        }
        Ok(())
    }

    // ==================== 全局声明 ====================

    fn gen_global_decl(&mut self, decl: &ast::Decl) -> IRResult<()> {
        match decl {
            ast::Decl::Const(const_decl) => self.gen_global_const_decl(const_decl),
            ast::Decl::Var(var_decl) => self.gen_global_var_decl(var_decl),
        }
    }

    fn gen_global_const_decl(&mut self, const_decl: &ast::ConstDecl) -> IRResult<()> {
        let base_ty = self.convert_type(&const_decl.btype);
        
        for def in &const_decl.defs {
            let ty = if def.dims.is_empty() {
                base_ty.clone()
            } else {
                // 构建数组类型
                let mut ty = base_ty.clone();
                for dim_expr in def.dims.iter().rev() {
                    let size = self.eval_const_expr(dim_expr)?;
                    ty = Type::array(ty, size as usize);
                }
                ty
            };
            
            let init = self.eval_const_init_val(&def.init)?;
            
            let global = GlobalVar {
                name: def.ident.clone(),
                ty: ty.clone(),
                is_const: true,
                init: Some(init),
            };
            
            self.module.global_vars.push(global);
            self.define(def.ident.clone(), Value::Global(def.ident.clone()));
        }
        
        Ok(())
    }

    fn gen_global_var_decl(&mut self, var_decl: &ast::VarDecl) -> IRResult<()> {
        let base_ty = self.convert_type(&var_decl.btype);
        
        for def in &var_decl.defs {
            let ty = if def.dims.is_empty() {
                base_ty.clone()
            } else {
                let mut ty = base_ty.clone();
                for dim_expr in def.dims.iter().rev() {
                    let size = self.eval_const_expr(dim_expr)?;
                    ty = Type::array(ty, size as usize);
                }
                ty
            };
            
            let init = if let Some(init_val) = &def.init {
                Some(self.eval_init_val(init_val)?)
            } else {
                None
            };
            
            let global = GlobalVar {
                name: def.ident.clone(),
                ty: ty.clone(),
                is_const: false,
                init,
            };
            
            self.module.global_vars.push(global);
            self.define(def.ident.clone(), Value::Global(def.ident.clone()));
        }
        
        Ok(())
    }

    // ==================== 函数 ====================

    fn gen_func_def(&mut self, func_def: &ast::FuncDef) -> IRResult<()> {
        let return_ty = self.convert_type(&func_def.return_type);
        
        // 构建参数列表
        let params: Vec<Parameter> = func_def.params.iter()
            .map(|p| Parameter {
                name: p.ident.clone(),
                ty: self.convert_type(&p.btype),
            })
            .collect();
        
        // 创建函数
        let func = Function::new(func_def.ident.clone(), params, return_ty);
        self.module.functions.push(func);
        self.current_function = Some(self.module.functions.len() - 1);
        
        // 重置寄存器和标签计数器
        self.reg_counter = 0;
        self.label_counter = 0;
        
        // 创建入口基本块
        let entry_label = "entry".to_string();
        self.create_block(entry_label.clone());
        self.switch_to_block(entry_label);
        
        // 进入函数作用域
        self.enter_scope();
        
        // 为参数分配栈空间并存储
        for (i, param) in func_def.params.iter().enumerate() {
            let param_val = Value::Param(i);
            let ptr_reg = self.new_reg(Type::ptr_to(self.convert_type(&param.btype)));
            
            // alloca
            self.emit(Instruction::Alloca {
                result: ptr_reg.clone(),
                ty: self.convert_type(&param.btype),
            });
            
            // store param to alloca
            self.emit(Instruction::Store {
                value: param_val,
                ptr: Value::Reg(ptr_reg.clone()),
            });
            
            // 记录到符号表
            self.define(param.ident.clone(), Value::Reg(ptr_reg));
        }
        
        // 生成函数体
        self.gen_block(&func_def.block)?;
        
        // 如果最后一个基本块没有终结指令，添加默认的 return
        if self.current_bb_mut().terminator.is_none() {
            if func_def.return_type == ast::BType::Void {
                self.emit_terminator(Terminator::Return(None));
            } else {
                // 非 void 函数缺少 return，返回 0
                self.emit_terminator(Terminator::Return(Some(Value::Const(Constant::Int(0)))));
            }
        }
        
        self.exit_scope();
        self.current_function = None;
        self.current_block = None;
        
        Ok(())
    }

    // ==================== 语句块 ====================

    fn gen_block(&mut self, block: &ast::Block) -> IRResult<()> {
        self.enter_scope();
        
        for item in &block.items {
            match item {
                ast::BlockItem::Decl(decl) => self.gen_local_decl(decl)?,
                ast::BlockItem::Stmt(stmt) => self.gen_stmt(stmt)?,
            }
        }
        
        self.exit_scope();
        Ok(())
    }

    // ==================== 局部声明 ====================

    fn gen_local_decl(&mut self, decl: &ast::Decl) -> IRResult<()> {
        match decl {
            ast::Decl::Const(const_decl) => self.gen_local_const_decl(const_decl),
            ast::Decl::Var(var_decl) => self.gen_local_var_decl(var_decl),
        }
    }

    fn gen_local_const_decl(&mut self, const_decl: &ast::ConstDecl) -> IRResult<()> {
        let base_ty = self.convert_type(&const_decl.btype);
        
        for def in &const_decl.defs {
            if def.dims.is_empty() {
                // 标量常量：直接求值
                let val = self.eval_const_expr_from_init(&def.init)?;
                self.define(def.ident.clone(), Value::Const(Constant::Int(val)));
            } else {
                // 数组常量：分配栈空间
                let mut ty = base_ty.clone();
                for dim_expr in def.dims.iter().rev() {
                    let size = self.eval_const_expr(dim_expr)?;
                    ty = Type::array(ty, size as usize);
                }
                
                let ptr_reg = self.new_reg(Type::ptr_to(ty.clone()));
                self.emit(Instruction::Alloca {
                    result: ptr_reg.clone(),
                    ty,
                });
                
                // TODO: 初始化数组
                
                self.define(def.ident.clone(), Value::Reg(ptr_reg));
            }
        }
        
        Ok(())
    }

    fn gen_local_var_decl(&mut self, var_decl: &ast::VarDecl) -> IRResult<()> {
        let base_ty = self.convert_type(&var_decl.btype);
        
        for def in &var_decl.defs {
            let ty = if def.dims.is_empty() {
                base_ty.clone()
            } else {
                let mut ty = base_ty.clone();
                for dim_expr in def.dims.iter().rev() {
                    let size = self.eval_const_expr(dim_expr)?;
                    ty = Type::array(ty, size as usize);
                }
                ty
            };
            
            // alloca
            let ptr_reg = self.new_reg(Type::ptr_to(ty.clone()));
            self.emit(Instruction::Alloca {
                result: ptr_reg.clone(),
                ty,
            });
            
            // 初始化（如果有）
            if let Some(init) = &def.init {
                let init_val = self.gen_init_val(init)?;
                self.emit(Instruction::Store {
                    value: init_val,
                    ptr: Value::Reg(ptr_reg.clone()),
                });
            }
            
            self.define(def.ident.clone(), Value::Reg(ptr_reg));
        }
        
        Ok(())
    }

    // ==================== 语句 ====================

    fn gen_stmt(&mut self, stmt: &ast::Stmt) -> IRResult<()> {
        match stmt {
            ast::Stmt::Assign { lval, expr, .. } => {
                let val = self.gen_expr(expr)?;
                let ptr = self.gen_lval_ptr(lval)?;
                self.emit(Instruction::Store {
                    value: val,
                    ptr,
                });
                Ok(())
            }
            
            ast::Stmt::Expr { expr, .. } => {
                if let Some(e) = expr {
                    self.gen_expr(e)?;
                }
                Ok(())
            }
            
            ast::Stmt::Block(block) => {
                self.gen_block(block)
            }
            
            ast::Stmt::If { cond, then_stmt, else_stmt, .. } => {
                self.gen_if_stmt(cond, then_stmt, else_stmt.as_deref())
            }
            
            ast::Stmt::While { cond, body, .. } => {
                self.gen_while_stmt(cond, body)
            }
            
            ast::Stmt::Break { .. } => {
                let loop_ctx = self.loop_stack.last()
                    .ok_or_else(|| "break outside loop".to_string())?;
                let break_label = loop_ctx.break_label.clone();
                self.emit_terminator(Terminator::Jump(break_label));
                
                // 创建一个不可达的后续块
                let unreachable_label = self.new_label("unreachable");
                self.create_block(unreachable_label.clone());
                self.switch_to_block(unreachable_label);
                
                Ok(())
            }
            
            ast::Stmt::Continue { .. } => {
                let loop_ctx = self.loop_stack.last()
                    .ok_or_else(|| "continue outside loop".to_string())?;
                let continue_label = loop_ctx.continue_label.clone();
                self.emit_terminator(Terminator::Jump(continue_label));
                
                // 创建一个不可达的后续块
                let unreachable_label = self.new_label("unreachable");
                self.create_block(unreachable_label.clone());
                self.switch_to_block(unreachable_label);
                
                Ok(())
            }
            
            ast::Stmt::Return { expr, .. } => {
                let ret_val = if let Some(e) = expr {
                    Some(self.gen_expr(e)?)
                } else {
                    None
                };
                self.emit_terminator(Terminator::Return(ret_val));
                
                // 创建一个不可达的后续块
                let unreachable_label = self.new_label("unreachable");
                self.create_block(unreachable_label.clone());
                self.switch_to_block(unreachable_label);
                
                Ok(())
            }
        }
    }

    fn gen_if_stmt(&mut self, cond: &ast::Expr, then_stmt: &ast::Stmt, else_stmt: Option<&ast::Stmt>) -> IRResult<()> {
        let cond_val = self.gen_expr(cond)?;
        
        let then_label = self.new_label("if.then");
        let else_label = self.new_label("if.else");
        let end_label = self.new_label("if.end");
        
        // 条件分支
        self.emit_terminator(Terminator::Branch {
            cond: cond_val,
            true_label: then_label.clone(),
            false_label: if else_stmt.is_some() { else_label.clone() } else { end_label.clone() },
        });
        
        // then 块
        self.create_block(then_label.clone());
        self.switch_to_block(then_label);
        self.gen_stmt(then_stmt)?;
        if self.current_bb_mut().terminator.is_none() {
            self.emit_terminator(Terminator::Jump(end_label.clone()));
        }
        
        // else 块（如果有）
        if let Some(else_s) = else_stmt {
            self.create_block(else_label.clone());
            self.switch_to_block(else_label);
            self.gen_stmt(else_s)?;
            if self.current_bb_mut().terminator.is_none() {
                self.emit_terminator(Terminator::Jump(end_label.clone()));
            }
        }
        
        // end 块
        self.create_block(end_label.clone());
        self.switch_to_block(end_label);
        
        Ok(())
    }

    fn gen_while_stmt(&mut self, cond: &ast::Expr, body: &ast::Stmt) -> IRResult<()> {
        let cond_label = self.new_label("while.cond");
        let body_label = self.new_label("while.body");
        let end_label = self.new_label("while.end");
        
        // 跳转到条件块
        self.emit_terminator(Terminator::Jump(cond_label.clone()));
        
        // 条件块
        self.create_block(cond_label.clone());
        self.switch_to_block(cond_label.clone());
        let cond_val = self.gen_expr(cond)?;
        self.emit_terminator(Terminator::Branch {
            cond: cond_val,
            true_label: body_label.clone(),
            false_label: end_label.clone(),
        });
        
        // 循环体块
        self.create_block(body_label.clone());
        self.switch_to_block(body_label);
        
        // 压入循环上下文
        self.loop_stack.push(LoopContext {
            continue_label: cond_label.clone(),
            break_label: end_label.clone(),
        });
        
        self.gen_stmt(body)?;
        
        self.loop_stack.pop();
        
        if self.current_bb_mut().terminator.is_none() {
            self.emit_terminator(Terminator::Jump(cond_label));
        }
        
        // end 块
        self.create_block(end_label.clone());
        self.switch_to_block(end_label);
        
        Ok(())
    }

    // ==================== 表达式 ====================

    fn gen_expr(&mut self, expr: &ast::Expr) -> IRResult<Value> {
        match expr {
            ast::Expr::Number { value, .. } => {
                Ok(Value::Const(Constant::Int(*value)))
            }
            
            ast::Expr::LVal(lval) => {
                let ptr = self.gen_lval_ptr(lval)?;
                let result = self.new_reg(Type::Int);  // TODO: 获取正确的类型
                self.emit(Instruction::Load {
                    result: result.clone(),
                    ptr,
                });
                Ok(Value::Reg(result))
            }
            
            ast::Expr::Binary { op, left, right, .. } => {
                let left_val = self.gen_expr(left)?;
                let right_val = self.gen_expr(right)?;
                let ir_op = self.convert_binary_op(op);
                let result = self.new_reg(Type::Int);  // TODO: 根据运算符确定类型
                self.emit(Instruction::Binary {
                    result: result.clone(),
                    op: ir_op,
                    left: left_val,
                    right: right_val,
                });
                Ok(Value::Reg(result))
            }
            
            ast::Expr::Unary { op, expr, .. } => {
                if let Some(ir_op) = self.convert_unary_op(op) {
                    let operand = self.gen_expr(expr)?;
                    let result = self.new_reg(Type::Int);
                    self.emit(Instruction::Unary {
                        result: result.clone(),
                        op: ir_op,
                        operand,
                    });
                    Ok(Value::Reg(result))
                } else {
                    // +x 直接返回 x
                    self.gen_expr(expr)
                }
            }
            
            ast::Expr::Call { ident, args, .. } => {
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.gen_expr(arg)?);
                }
                
                // TODO: 从函数签名获取返回类型
                let has_return = ident != "void";  // 简化处理
                let result = if has_return {
                    Some(self.new_reg(Type::Int))
                } else {
                    None
                };
                
                self.emit(Instruction::Call {
                    result: result.clone(),
                    func: ident.clone(),
                    args: arg_vals,
                });
                
                if let Some(res) = result {
                    Ok(Value::Reg(res))
                } else {
                    Ok(Value::Const(Constant::Void))
                }
            }
        }
    }

    /// 生成左值的指针
    fn gen_lval_ptr(&mut self, lval: &ast::LVal) -> IRResult<Value> {
        let base = self.lookup(&lval.ident)
            .ok_or_else(|| format!("Undefined variable: {}", lval.ident))?
            .clone();
        
        if lval.indices.is_empty() {
            // 简单变量
            Ok(base)
        } else {
            // 数组元素
            let mut indices = Vec::new();
            for idx_expr in &lval.indices {
                indices.push(self.gen_expr(idx_expr)?);
            }
            
            let result = self.new_reg(Type::ptr_to(Type::Int));  // TODO: 正确的类型
            self.emit(Instruction::GetElementPtr {
                result: result.clone(),
                base,
                indices,
            });
            Ok(Value::Reg(result))
        }
    }

    // ==================== 初始值 ====================

    fn gen_init_val(&mut self, init: &ast::InitVal) -> IRResult<Value> {
        match init {
            ast::InitVal::Expr(expr) => self.gen_expr(expr),
            ast::InitVal::List(list) => {
                // 数组初始化：简化处理，只返回第一个元素
                // 完整实现需要递归处理多维数组
                if list.is_empty() {
                    Ok(Value::Const(Constant::Int(0)))
                } else {
                    self.gen_init_val(&list[0])
                }
            }
        }
    }

    // ==================== 常量求值 ====================

    fn eval_const_expr(&self, expr: &ast::Expr) -> IRResult<i64> {
        match expr {
            ast::Expr::Number { value, .. } => Ok(*value),
            ast::Expr::Binary { op, left, right, .. } => {
                let left_val = self.eval_const_expr(left)?;
                let right_val = self.eval_const_expr(right)?;
                Ok(match op {
                    ast::BinaryOp::Add => left_val + right_val,
                    ast::BinaryOp::Sub => left_val - right_val,
                    ast::BinaryOp::Mul => left_val * right_val,
                    ast::BinaryOp::Div => left_val / right_val,
                    ast::BinaryOp::Mod => left_val % right_val,
                    _ => return Err("Non-arithmetic operator in const expr".to_string()),
                })
            }
            ast::Expr::Unary { op, expr, .. } => {
                let val = self.eval_const_expr(expr)?;
                Ok(match op {
                    ast::UnaryOp::Pos => val,
                    ast::UnaryOp::Neg => -val,
                    ast::UnaryOp::Not => if val == 0 { 1 } else { 0 },
                })
            }
            _ => Err("Non-constant expression".to_string()),
        }
    }

    fn eval_const_expr_from_init(&self, init: &ast::ConstInitVal) -> IRResult<i64> {
        match init {
            ast::ConstInitVal::Expr(expr) => self.eval_const_expr(expr),
            _ => Err("Expected scalar initializer".to_string()),
        }
    }

    fn eval_const_init_val(&self, init: &ast::ConstInitVal) -> IRResult<Constant> {
        match init {
            ast::ConstInitVal::Expr(expr) => {
                let val = self.eval_const_expr(expr)?;
                Ok(Constant::Int(val))
            }
            ast::ConstInitVal::List(list) => {
                let mut vals = Vec::new();
                for item in list {
                    vals.push(self.eval_const_init_val(item)?);
                }
                Ok(Constant::Array(vals))
            }
        }
    }

    fn eval_init_val(&self, init: &ast::InitVal) -> IRResult<Constant> {
        match init {
            ast::InitVal::Expr(expr) => {
                let val = self.eval_const_expr(expr)?;
                Ok(Constant::Int(val))
            }
            ast::InitVal::List(list) => {
                let mut vals = Vec::new();
                for item in list {
                    vals.push(self.eval_init_val(item)?);
                }
                Ok(Constant::Array(vals))
            }
        }
    }
}

