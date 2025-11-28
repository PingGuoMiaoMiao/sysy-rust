// src/semantic.rs
// 语义分析器：作用域管理、未定义符号检查、函数参数检查

use crate::ast::*;
use std::collections::HashMap;

pub struct SemanticAnalyzer {
    /// 符号表栈（每层代表一个作用域）
    scopes: Vec<HashMap<String, SymbolInfo>>,
    /// 当前函数信息（用于检查 break/continue/return）
    current_function: Option<FunctionInfo>,
    /// 循环嵌套深度（用于检查 break/continue）
    loop_depth: usize,
    /// 错误列表
    errors: Vec<SemanticError>,
}

#[derive(Debug, Clone)]
struct SymbolInfo {
    kind: SymbolKind,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
enum SymbolKind {
    Const,
    Var,
    Function { param_count: usize },
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    name: String,
    return_type: BType,
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub message: String,
    pub span: Span,
}

impl SemanticError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        SemanticError {
            message: message.into(),
            span,
        }
    }
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            scopes: vec![HashMap::new()],  // 全局作用域
            current_function: None,
            loop_depth: 0,
            errors: Vec::new(),
        }
    }

    /// 执行语义分析
    pub fn analyze(&mut self, comp_unit: &CompUnit) -> Result<(), Vec<SemanticError>> {
        self.analyze_comp_unit(comp_unit);
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    /// 进入新作用域
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// 退出作用域
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// 查找符号
    fn lookup(&self, name: &str) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    /// 在当前作用域定义符号
    fn define(&mut self, name: String, kind: SymbolKind, span: Span) {
        let current_scope = self.scopes.last_mut().unwrap();
        
        if let Some(existing) = current_scope.get(&name) {
            self.errors.push(SemanticError::new(
                format!(
                    "Symbol '{}' is already defined in this scope (previously at line {})",
                    name, existing.span.line
                ),
                span,
            ));
        } else {
            current_scope.insert(name, SymbolInfo { kind, span });
        }
    }

    /// 记录错误
    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(SemanticError::new(message, span));
    }

    // ==================== 分析方法 ====================

    fn analyze_comp_unit(&mut self, comp_unit: &CompUnit) {
        // 第一遍：收集所有全局声明和函数签名
        for item in &comp_unit.items {
            match item {
                CompUnitItem::Decl(decl) => self.collect_decl(decl),
                CompUnitItem::FuncDef(func_def) => self.collect_func_def(func_def),
            }
        }

        // 第二遍：检查函数体
        for item in &comp_unit.items {
            if let CompUnitItem::FuncDef(func_def) = item {
                self.analyze_func_def(func_def);
            }
        }

        // 检查是否有 main 函数
        if self.lookup("main").is_none() {
            self.error(
                "Program must have a 'main' function",
                Span::new(1, 0),
            );
        }
    }

    fn collect_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Const(const_decl) => {
                for def in &const_decl.defs {
                    self.define(def.ident.clone(), SymbolKind::Const, def.span.clone());
                }
            }
            Decl::Var(var_decl) => {
                for def in &var_decl.defs {
                    self.define(def.ident.clone(), SymbolKind::Var, def.span.clone());
                }
            }
        }
    }

    fn collect_func_def(&mut self, func_def: &FuncDef) {
        self.define(
            func_def.ident.clone(),
            SymbolKind::Function {
                param_count: func_def.params.len(),
            },
            func_def.span.clone(),
        );
    }

    fn analyze_func_def(&mut self, func_def: &FuncDef) {
        // 设置当前函数上下文
        self.current_function = Some(FunctionInfo {
            name: func_def.ident.clone(),
            return_type: func_def.return_type.clone(),
        });

        // 进入函数作用域
        self.enter_scope();

        // 添加参数到符号表
        for param in &func_def.params {
            self.define(
                param.ident.clone(),
                SymbolKind::Var,
                param.span.clone(),
            );
        }

        // 检查参数名是否重复
        let mut param_names = std::collections::HashSet::new();
        for param in &func_def.params {
            if !param_names.insert(&param.ident) {
                self.error(
                    format!("Duplicate parameter name '{}'", param.ident),
                    param.span.clone(),
                );
            }
        }

        // 分析函数体
        self.analyze_block(&func_def.block);

        // 检查非 void 函数是否有返回语句
        if func_def.return_type != BType::Void {
            if !self.block_has_return(&func_def.block) {
                self.error(
                    format!(
                        "Function '{}' with non-void return type must return a value",
                        func_def.ident
                    ),
                    func_def.span.clone(),
                );
            }
        }

        self.exit_scope();
        self.current_function = None;
    }

    fn analyze_block(&mut self, block: &Block) {
        self.enter_scope();

        for item in &block.items {
            match item {
                BlockItem::Decl(decl) => self.analyze_decl(decl),
                BlockItem::Stmt(stmt) => self.analyze_stmt(stmt),
            }
        }

        self.exit_scope();
    }

    fn analyze_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Const(const_decl) => {
                for def in &const_decl.defs {
                    // 检查数组维度表达式
                    for dim_expr in &def.dims {
                        self.analyze_expr(dim_expr);
                    }
                    
                    // 检查初始值表达式
                    self.analyze_const_init_val(&def.init);
                    
                    // 定义符号
                    self.define(def.ident.clone(), SymbolKind::Const, def.span.clone());
                }
            }
            Decl::Var(var_decl) => {
                for def in &var_decl.defs {
                    // 检查数组维度表达式
                    for dim_expr in &def.dims {
                        self.analyze_expr(dim_expr);
                    }
                    
                    // 检查初始值表达式
                    if let Some(init) = &def.init {
                        self.analyze_init_val(init);
                    }
                    
                    // 定义符号
                    self.define(def.ident.clone(), SymbolKind::Var, def.span.clone());
                }
            }
        }
    }

    fn analyze_const_init_val(&mut self, init: &ConstInitVal) {
        match init {
            ConstInitVal::Expr(expr) => self.analyze_expr(expr),
            ConstInitVal::List(list) => {
                for item in list {
                    self.analyze_const_init_val(item);
                }
            }
        }
    }

    fn analyze_init_val(&mut self, init: &InitVal) {
        match init {
            InitVal::Expr(expr) => self.analyze_expr(expr),
            InitVal::List(list) => {
                for item in list {
                    self.analyze_init_val(item);
                }
            }
        }
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign { lval, expr, span } => {
                self.analyze_lval(lval);
                self.analyze_expr(expr);
                
                // 检查左值是否是常量
                if let Some(info) = self.lookup(&lval.ident) {
                    if info.kind == SymbolKind::Const {
                        self.error(
                            format!("Cannot assign to constant '{}'", lval.ident),
                            span.clone(),
                        );
                    }
                }
            }
            Stmt::Expr { expr, .. } => {
                if let Some(e) = expr {
                    self.analyze_expr(e);
                }
            }
            Stmt::Block(block) => {
                self.analyze_block(block);
            }
            Stmt::If { cond, then_stmt, else_stmt, .. } => {
                self.analyze_expr(cond);
                self.analyze_stmt(then_stmt);
                if let Some(else_s) = else_stmt {
                    self.analyze_stmt(else_s);
                }
            }
            Stmt::While { cond, body, .. } => {
                self.analyze_expr(cond);
                self.loop_depth += 1;
                self.analyze_stmt(body);
                self.loop_depth -= 1;
            }
            Stmt::Break { span } => {
                if self.loop_depth == 0 {
                    self.error("'break' statement not in loop", span.clone());
                }
            }
            Stmt::Continue { span } => {
                if self.loop_depth == 0 {
                    self.error("'continue' statement not in loop", span.clone());
                }
            }
            Stmt::Return { expr, span } => {
                if self.current_function.is_none() {
                    self.error("'return' statement outside function", span.clone());
                    return;
                }

                let func_info = self.current_function.as_ref().unwrap();
                
                match (expr, &func_info.return_type) {
                    (Some(e), BType::Void) => {
                        self.error(
                            "Void function should not return a value",
                            span.clone(),
                        );
                        self.analyze_expr(e);
                    }
                    (None, BType::Int) => {
                        self.error(
                            "Non-void function must return a value",
                            span.clone(),
                        );
                    }
                    (Some(e), _) => {
                        self.analyze_expr(e);
                    }
                    (None, BType::Void) => {}
                }
            }
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary { left, right, .. } => {
                self.analyze_expr(left);
                self.analyze_expr(right);
            }
            Expr::Unary { expr, .. } => {
                self.analyze_expr(expr);
            }
            Expr::Call { ident, args, span } => {
                // 检查函数是否存在
                match self.lookup(ident) {
                    Some(SymbolInfo { kind: SymbolKind::Function { param_count }, .. }) => {
                        // 检查参数数量
                        if args.len() != *param_count {
                            self.error(
                                format!(
                                    "Function '{}' expects {} arguments, but {} were provided",
                                    ident, param_count, args.len()
                                ),
                                span.clone(),
                            );
                        }
                    }
                    Some(info) => {
                        self.error(
                            format!("'{}' is not a function (defined at line {})", ident, info.span.line),
                            span.clone(),
                        );
                    }
                    None => {
                        self.error(
                            format!("Undefined function '{}'", ident),
                            span.clone(),
                        );
                    }
                }
                
                // 检查参数表达式
                for arg in args {
                    self.analyze_expr(arg);
                }
            }
            Expr::LVal(lval) => {
                self.analyze_lval(lval);
            }
            Expr::Number { .. } => {}
        }
    }

    fn analyze_lval(&mut self, lval: &LVal) {
        // 检查变量是否定义
        if self.lookup(&lval.ident).is_none() {
            self.error(
                format!("Undefined variable '{}'", lval.ident),
                lval.span.clone(),
            );
        }

        // 检查数组索引表达式
        for index in &lval.indices {
            self.analyze_expr(index);
        }
    }

    /// 检查语句块是否包含 return 语句（简化版本）
    fn block_has_return(&self, block: &Block) -> bool {
        for item in &block.items {
            if let BlockItem::Stmt(stmt) = item {
                if self.stmt_has_return(stmt) {
                    return true;
                }
            }
        }
        false
    }

    /// 检查语句是否包含 return（简化版本）
    fn stmt_has_return(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Return { .. } => true,
            Stmt::Block(block) => self.block_has_return(block),
            Stmt::If { then_stmt, else_stmt, .. } => {
                // 只有当 if 和 else 都有 return 时才算有 return
                self.stmt_has_return(then_stmt) 
                    && else_stmt.as_ref().map_or(false, |s| self.stmt_has_return(s))
            }
            Stmt::While { body, .. } => {
                // while 循环中的 return 不算（可能不执行）
                self.stmt_has_return(body)
            }
            _ => false,
        }
    }
}

