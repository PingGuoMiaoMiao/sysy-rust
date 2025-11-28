// src/type_checker.rs
// 类型检查器：支持整型、数组、函数类型推断与匹配

use crate::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Void,
    Array {
        elem_type: Box<Type>,
        dims: Vec<usize>,  // 每一维的大小
    },
    Function {
        return_type: Box<Type>,
        param_types: Vec<Type>,
    },
}

impl Type {
    /// 检查两个类型是否兼容
    pub fn is_compatible(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Void, Type::Void) => true,
            (Type::Array { elem_type: e1, dims: d1 }, Type::Array { elem_type: e2, dims: d2 }) => {
                e1.is_compatible(e2) && d1 == d2
            }
            (Type::Function { return_type: r1, param_types: p1 }, 
             Type::Function { return_type: r2, param_types: p2 }) => {
                r1.is_compatible(r2) 
                    && p1.len() == p2.len()
                    && p1.iter().zip(p2.iter()).all(|(t1, t2)| t1.is_compatible(t2))
            }
            _ => false,
        }
    }

    /// 获取数组元素类型
    pub fn array_elem_type(&self) -> Option<&Type> {
        match self {
            Type::Array { elem_type, .. } => Some(elem_type),
            _ => None,
        }
    }

    /// 检查是否是整数类型（用于运算）
    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    /// 检查是否是 void 类型
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }
}

pub struct TypeChecker {
    /// 符号表：变量名 -> 类型
    symbol_table: Vec<HashMap<String, Type>>,
    /// 当前函数的返回类型
    current_func_return_type: Option<Type>,
    /// 错误列表
    errors: Vec<TypeError>,
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl TypeError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        TypeError {
            message: message.into(),
            span,
        }
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            symbol_table: vec![HashMap::new()],  // 全局作用域
            current_func_return_type: None,
            errors: Vec::new(),
        }
    }

    /// 进入新作用域
    fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    /// 退出作用域
    fn exit_scope(&mut self) {
        self.symbol_table.pop();
    }

    /// 查找符号类型
    fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    /// 在当前作用域插入符号
    fn insert(&mut self, name: String, ty: Type) -> Result<(), String> {
        let current_scope = self.symbol_table.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            Err(format!("Symbol '{}' is already defined in current scope", name))
        } else {
            current_scope.insert(name, ty);
            Ok(())
        }
    }

    /// 记录错误
    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(TypeError::new(message, span));
    }

    /// 执行类型检查
    pub fn check(&mut self, comp_unit: &CompUnit) -> Result<(), Vec<TypeError>> {
        self.check_comp_unit(comp_unit);
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_comp_unit(&mut self, comp_unit: &CompUnit) {
        for item in &comp_unit.items {
            match item {
                CompUnitItem::Decl(decl) => self.check_decl(decl),
                CompUnitItem::FuncDef(func_def) => self.check_func_def(func_def),
            }
        }
    }

    fn check_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Const(const_decl) => self.check_const_decl(const_decl),
            Decl::Var(var_decl) => self.check_var_decl(var_decl),
        }
    }

    fn check_const_decl(&mut self, const_decl: &ConstDecl) {
        let base_type = self.btype_to_type(&const_decl.btype);
        
        for def in &const_decl.defs {
            let ty = if def.dims.is_empty() {
                base_type.clone()
            } else {
                // 检查数组维度表达式
                let mut dims = Vec::new();
                for dim_expr in &def.dims {
                    if let Some(size) = self.eval_const_expr(dim_expr) {
                        if size <= 0 {
                            self.error(
                                format!("Array dimension must be positive, got {}", size),
                                def.span.clone(),
                            );
                        }
                        dims.push(size as usize);
                    } else {
                        self.error("Array dimension must be a constant expression", def.span.clone());
                    }
                }
                Type::Array {
                    elem_type: Box::new(base_type.clone()),
                    dims,
                }
            };

            // 检查初始值类型
            self.check_const_init_val(&def.init, &ty, &def.span);

            // 插入符号表
            if let Err(e) = self.insert(def.ident.clone(), ty) {
                self.error(e, def.span.clone());
            }
        }
    }

    fn check_var_decl(&mut self, var_decl: &VarDecl) {
        let base_type = self.btype_to_type(&var_decl.btype);
        
        for def in &var_decl.defs {
            let ty = if def.dims.is_empty() {
                base_type.clone()
            } else {
                let mut dims = Vec::new();
                for dim_expr in &def.dims {
                    if let Some(size) = self.eval_const_expr(dim_expr) {
                        if size <= 0 {
                            self.error(
                                format!("Array dimension must be positive, got {}", size),
                                def.span.clone(),
                            );
                        }
                        dims.push(size as usize);
                    } else {
                        self.error("Array dimension must be a constant expression", def.span.clone());
                    }
                }
                Type::Array {
                    elem_type: Box::new(base_type.clone()),
                    dims,
                }
            };

            // 检查初始值类型（如果有）
            if let Some(init) = &def.init {
                self.check_init_val(init, &ty, &def.span);
            }

            // 插入符号表
            if let Err(e) = self.insert(def.ident.clone(), ty) {
                self.error(e, def.span.clone());
            }
        }
    }

    fn check_const_init_val(&mut self, init: &ConstInitVal, expected_ty: &Type, span: &Span) {
        match (init, expected_ty) {
            (ConstInitVal::Expr(expr), Type::Int) => {
                let expr_ty = self.infer_expr_type(expr);
                if !expr_ty.is_compatible(&Type::Int) {
                    self.error(
                        format!("Type mismatch: expected Int, found {:?}", expr_ty),
                        span.clone(),
                    );
                }
            }
            (ConstInitVal::List(list), Type::Array { elem_type, dims }) => {
                // 简化处理：检查列表长度
                if !list.is_empty() && dims.is_empty() {
                    self.error("Cannot initialize non-array with list", span.clone());
                }
                // 递归检查每个元素
                for item in list {
                    self.check_const_init_val(item, elem_type, span);
                }
            }
            _ => {
                self.error(
                    format!("Invalid initializer for type {:?}", expected_ty),
                    span.clone(),
                );
            }
        }
    }

    fn check_init_val(&mut self, init: &InitVal, expected_ty: &Type, span: &Span) {
        match (init, expected_ty) {
            (InitVal::Expr(expr), Type::Int) => {
                let expr_ty = self.infer_expr_type(expr);
                if !expr_ty.is_compatible(&Type::Int) {
                    self.error(
                        format!("Type mismatch: expected Int, found {:?}", expr_ty),
                        span.clone(),
                    );
                }
            }
            (InitVal::List(list), Type::Array { elem_type, dims }) => {
                if !list.is_empty() && dims.is_empty() {
                    self.error("Cannot initialize non-array with list", span.clone());
                }
                for item in list {
                    self.check_init_val(item, elem_type, span);
                }
            }
            _ => {
                self.error(
                    format!("Invalid initializer for type {:?}", expected_ty),
                    span.clone(),
                );
            }
        }
    }

    fn check_func_def(&mut self, func_def: &FuncDef) {
        let return_type = self.btype_to_type(&func_def.return_type);
        
        // 构建参数类型列表
        let mut param_types = Vec::new();
        for param in &func_def.params {
            let base_type = self.btype_to_type(&param.btype);
            let param_type = if param.dims.is_empty() {
                base_type
            } else {
                // 函数参数数组：第一维可以为空
                let dims = vec![0; param.dims.len()];  // 简化处理
                Type::Array {
                    elem_type: Box::new(base_type),
                    dims,
                }
            };
            param_types.push(param_type);
        }

        // 注册函数类型
        let func_type = Type::Function {
            return_type: Box::new(return_type.clone()),
            param_types,
        };

        if let Err(e) = self.insert(func_def.ident.clone(), func_type) {
            self.error(e, func_def.span.clone());
        }

        // 进入函数作用域
        self.enter_scope();
        self.current_func_return_type = Some(return_type);

        // 添加参数到符号表
        for param in &func_def.params {
            let base_type = self.btype_to_type(&param.btype);
            let param_type = if param.dims.is_empty() {
                base_type
            } else {
                let dims = vec![0; param.dims.len()];
                Type::Array {
                    elem_type: Box::new(base_type),
                    dims,
                }
            };
            if let Err(e) = self.insert(param.ident.clone(), param_type) {
                self.error(e, param.span.clone());
            }
        }

        // 检查函数体
        self.check_block(&func_def.block);

        self.current_func_return_type = None;
        self.exit_scope();
    }

    fn check_block(&mut self, block: &Block) {
        self.enter_scope();

        for item in &block.items {
            match item {
                BlockItem::Decl(decl) => self.check_decl(decl),
                BlockItem::Stmt(stmt) => self.check_stmt(stmt),
            }
        }

        self.exit_scope();
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign { lval, expr, span } => {
                let lval_ty = self.infer_lval_type(lval);
                let expr_ty = self.infer_expr_type(expr);
                
                if !lval_ty.is_compatible(&expr_ty) {
                    self.error(
                        format!("Type mismatch in assignment: {:?} vs {:?}", lval_ty, expr_ty),
                        span.clone(),
                    );
                }
            }
            Stmt::Expr { expr, .. } => {
                if let Some(e) = expr {
                    self.infer_expr_type(e);
                }
            }
            Stmt::Block(block) => self.check_block(block),
            Stmt::If { cond, then_stmt, else_stmt, span } => {
                let cond_ty = self.infer_expr_type(cond);
                if !cond_ty.is_int() {
                    self.error("Condition must be of integer type", span.clone());
                }
                self.check_stmt(then_stmt);
                if let Some(else_s) = else_stmt {
                    self.check_stmt(else_s);
                }
            }
            Stmt::While { cond, body, span } => {
                let cond_ty = self.infer_expr_type(cond);
                if !cond_ty.is_int() {
                    self.error("Condition must be of integer type", span.clone());
                }
                self.check_stmt(body);
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {
                // TODO: 检查是否在循环中
            }
            Stmt::Return { expr, span } => {
                // 先 clone return type，避免借用冲突
                if let Some(return_ty) = self.current_func_return_type.clone() {
                    match (expr, &return_ty) {
                        (Some(e), ty) => {
                            let expr_ty = self.infer_expr_type(e);
                            if !expr_ty.is_compatible(ty) {
                                self.error(
                                    format!("Return type mismatch: expected {:?}, found {:?}", ty, expr_ty),
                                    span.clone(),
                                );
                            }
                        }
                        (None, Type::Void) => {}
                        (None, ty) => {
                            self.error(
                                format!("Return value expected for non-void function (expected {:?})", ty),
                                span.clone(),
                            );
                        }
                    }
                }
            }
        }
    }

    /// 推断表达式类型
    fn infer_expr_type(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Binary { op: _, left, right, span } => {
                let left_ty = self.infer_expr_type(left);
                let right_ty = self.infer_expr_type(right);
                
                if !left_ty.is_int() || !right_ty.is_int() {
                    self.error(
                        format!("Binary operation requires integer operands, got {:?} and {:?}", left_ty, right_ty),
                        span.clone(),
                    );
                }
                Type::Int
            }
            Expr::Unary { op: _, expr, span } => {
                let expr_ty = self.infer_expr_type(expr);
                if !expr_ty.is_int() {
                    self.error(
                        format!("Unary operation requires integer operand, got {:?}", expr_ty),
                        span.clone(),
                    );
                }
                Type::Int
            }
            Expr::Call { ident, args, span } => {
                // 先获取函数类型信息，避免借用冲突
                let func_type_opt = self.lookup(ident).cloned();
                
                match func_type_opt {
                    Some(Type::Function { return_type, param_types }) => {
                        // 检查参数数量
                        if args.len() != param_types.len() {
                            self.error(
                                format!("Function '{}' expects {} arguments, got {}", ident, param_types.len(), args.len()),
                                span.clone(),
                            );
                        }
                        
                        // 先收集参数类型，然后再检查（避免借用冲突）
                        let arg_types: Vec<Type> = args.iter()
                            .map(|arg| self.infer_expr_type(arg))
                            .collect();
                        
                        // 检查参数类型
                        for (i, (arg_ty, expected_ty)) in arg_types.iter().zip(param_types.iter()).enumerate() {
                            if !arg_ty.is_compatible(expected_ty) {
                                self.error(
                                    format!("Argument {} type mismatch: expected {:?}, found {:?}", i + 1, expected_ty, arg_ty),
                                    span.clone(),
                                );
                            }
                        }
                        
                        (*return_type).clone()
                    }
                    Some(ty) => {
                        self.error(format!("'{}' is not a function (type: {:?})", ident, ty), span.clone());
                        Type::Int  // 返回默认类型以继续检查
                    }
                    None => {
                        self.error(format!("Undefined function '{}'", ident), span.clone());
                        Type::Int
                    }
                }
            }
            Expr::LVal(lval) => self.infer_lval_type(lval),
            Expr::Number { .. } => Type::Int,
        }
    }

    /// 推断左值类型
    fn infer_lval_type(&mut self, lval: &LVal) -> Type {
        // 先获取变量类型，避免借用冲突
        let var_type_opt = self.lookup(&lval.ident).cloned();
        
        match var_type_opt {
            Some(ty) => {
                let mut current_ty = ty;
                
                // 处理数组索引
                for (i, index_expr) in lval.indices.iter().enumerate() {
                    let index_ty = self.infer_expr_type(index_expr);
                    if !index_ty.is_int() {
                        self.error(
                            format!("Array index must be integer, got {:?}", index_ty),
                            lval.span.clone(),
                        );
                    }
                    
                    current_ty = match &current_ty {
                        Type::Array { elem_type, dims } => {
                            if i >= dims.len() {
                                self.error(
                                    format!("Too many indices for array (expected {})", dims.len()),
                                    lval.span.clone(),
                                );
                                break;
                            }
                            
                            if i == dims.len() - 1 {
                                // 最后一维，返回元素类型
                                (**elem_type).clone()
                            } else {
                                // 中间维度，返回子数组类型
                                let remaining_dims = dims[i + 1..].to_vec();
                                Type::Array {
                                    elem_type: elem_type.clone(),
                                    dims: remaining_dims,
                                }
                            }
                        }
                        _ => {
                            self.error(
                                format!("'{}' is not an array", lval.ident),
                                lval.span.clone(),
                            );
                            break;
                        }
                    };
                }
                
                current_ty
            }
            None => {
                self.error(format!("Undefined variable '{}'", lval.ident), lval.span.clone());
                Type::Int  // 返回默认类型
            }
        }
    }

    fn btype_to_type(&self, btype: &BType) -> Type {
        match btype {
            BType::Int => Type::Int,
            BType::Void => Type::Void,
        }
    }

    /// 尝试计算常量表达式的值（用于数组维度）
    fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        match expr {
            Expr::Number { value, .. } => Some(*value),
            Expr::Unary { op, expr, .. } => {
                let val = self.eval_const_expr(expr)?;
                match op {
                    UnaryOp::Pos => Some(val),
                    UnaryOp::Neg => Some(-val),
                    UnaryOp::Not => Some(if val == 0 { 1 } else { 0 }),
                }
            }
            Expr::Binary { op, left, right, .. } => {
                let left_val = self.eval_const_expr(left)?;
                let right_val = self.eval_const_expr(right)?;
                Some(match op {
                    BinaryOp::Add => left_val + right_val,
                    BinaryOp::Sub => left_val - right_val,
                    BinaryOp::Mul => left_val * right_val,
                    BinaryOp::Div if right_val != 0 => left_val / right_val,
                    BinaryOp::Mod if right_val != 0 => left_val % right_val,
                    _ => return None,
                })
            }
            _ => None,
        }
    }
}

