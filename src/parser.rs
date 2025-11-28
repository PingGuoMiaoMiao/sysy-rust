// 递归下降 Parser
use crate::ast::*;
use crate::tokens::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        ParseError {
            message: message.into(),
            span,
        }
    }
}

/// 语法分析器
pub struct Parser {
    tokens: Peekable<IntoIter<(Token, String, usize)>>,
    current_line: usize,
}

impl Parser {
    pub fn new(tokens: Vec<(Token, String, usize)>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
            current_line: 1,
        }
    }

    /// 获取当前位置信息
    fn current_span(&self) -> Span {
        Span::new(self.current_line, 0)
    }

    /// 查看下一个 token（不消费）
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek().map(|(token, _, _)| token)
    }

    /// 获取下一个 token（消费）
    fn next(&mut self) -> Option<(Token, String, usize)> {
        let token = self.tokens.next();
        if let Some((_, _, line)) = &token {
            self.current_line = *line;
        }
        token
    }

    /// 期望一个特定的 token
    fn expect(&mut self, expected: Token) -> ParseResult<(Token, String, usize)> {
        match self.next() {
            Some((token, text, line)) if token == expected => Ok((token, text, line)),
            Some((token, text, line)) => Err(ParseError::new(
                format!("Expected {:?}, found {:?} ('{}')", expected, token, text),
                Span::new(line, 0),
            )),
            None => Err(ParseError::new(
                format!("Expected {:?}, found EOF", expected),
                self.current_span(),
            )),
        }
    }

    /// 尝试匹配一个 token，如果匹配成功则消费
    fn try_match(&mut self, expected: Token) -> bool {
        if let Some(token) = self.peek() {
            if *token == expected {
                self.next();
                return true;
            }
        }
        false
    }

    // ==================== 入口解析 ====================

    /// 解析编译单元（程序入口）
    /// CompUnit -> (Decl | FuncDef)*
    pub fn parse_comp_unit(&mut self) -> ParseResult<CompUnit> {
        let mut items = Vec::new();

        while self.peek().is_some() {
            // 向前看判断是声明还是函数定义
            if self.is_func_def() {
                items.push(CompUnitItem::FuncDef(self.parse_func_def()?));
            } else {
                items.push(CompUnitItem::Decl(self.parse_decl()?));
            }
        }

        Ok(CompUnit { items })
    }

    /// 判断是否是函数定义
    /// 需要向前看：Type Ident '('
    fn is_func_def(&mut self) -> bool {
        // 保存当前位置
        let tokens: Vec<_> = self.tokens.clone().collect();
        
        // 尝试匹配 Type
        if !matches!(self.peek(), Some(Token::KeywordInt) | Some(Token::KeywordVoid)) {
            // 恢复
            self.tokens = tokens.into_iter().peekable();
            return false;
        }
        self.next();

        // 尝试匹配 Ident
        if !matches!(self.peek(), Some(Token::Identifier(_))) {
            // 恢复
            self.tokens = tokens.into_iter().peekable();
            return false;
        }
        self.next();

        // 检查是否是 '('
        let is_func = matches!(self.peek(), Some(Token::LParen));
        
        // 恢复位置
        self.tokens = tokens.into_iter().peekable();
        is_func
    }

    // ==================== 声明解析 ====================

    /// 解析声明
    /// Decl -> ConstDecl | VarDecl
    fn parse_decl(&mut self) -> ParseResult<Decl> {
        match self.peek() {
            Some(Token::KeywordConst) => Ok(Decl::Const(self.parse_const_decl()?)),
            _ => Ok(Decl::Var(self.parse_var_decl()?)),
        }
    }

    /// 解析常量声明
    /// ConstDecl -> 'const' BType ConstDef (',' ConstDef)* ';'
    fn parse_const_decl(&mut self) -> ParseResult<ConstDecl> {
        let span = self.current_span();
        self.expect(Token::KeywordConst)?;
        let btype = self.parse_btype()?;

        let mut defs = vec![self.parse_const_def()?];
        while self.try_match(Token::Comma) {
            defs.push(self.parse_const_def()?);
        }

        self.expect(Token::Semicolon)?;
        Ok(ConstDecl { btype, defs, span })
    }

    /// 解析常量定义
    /// ConstDef -> Ident ('[' Expr ']')* '=' ConstInitVal
    fn parse_const_def(&mut self) -> ParseResult<ConstDef> {
        let span = self.current_span();
        let ident = match self.next() {
            Some((Token::Identifier(id), _, _)) => id,
            _ => return Err(ParseError::new("Expected identifier", span)),
        };

        let mut dims = Vec::new();
        while self.try_match(Token::LBrackt) {
            dims.push(self.parse_expr()?);
            self.expect(Token::RBrackt)?;
        }

        self.expect(Token::Assign)?;
        let init = self.parse_const_init_val()?;

        Ok(ConstDef { ident, dims, init, span })
    }

    /// 解析常量初始值
    /// ConstInitVal -> Expr | '{' (ConstInitVal (',' ConstInitVal)*)? '}'
    fn parse_const_init_val(&mut self) -> ParseResult<ConstInitVal> {
        if self.try_match(Token::LBrace) {
            let mut vals = Vec::new();
            if !matches!(self.peek(), Some(Token::RBrace)) {
                vals.push(self.parse_const_init_val()?);
                while self.try_match(Token::Comma) {
                    vals.push(self.parse_const_init_val()?);
                }
            }
            self.expect(Token::RBrace)?;
            Ok(ConstInitVal::List(vals))
        } else {
            Ok(ConstInitVal::Expr(self.parse_expr()?))
        }
    }

    /// 解析变量声明
    /// VarDecl -> BType VarDef (',' VarDef)* ';'
    fn parse_var_decl(&mut self) -> ParseResult<VarDecl> {
        let span = self.current_span();
        let btype = self.parse_btype()?;

        let mut defs = vec![self.parse_var_def()?];
        while self.try_match(Token::Comma) {
            defs.push(self.parse_var_def()?);
        }

        self.expect(Token::Semicolon)?;
        Ok(VarDecl { btype, defs, span })
    }

    /// 解析变量定义
    /// VarDef -> Ident ('[' Expr ']')* ('=' InitVal)?
    fn parse_var_def(&mut self) -> ParseResult<VarDef> {
        let span = self.current_span();
        let ident = match self.next() {
            Some((Token::Identifier(id), _, _)) => id,
            _ => return Err(ParseError::new("Expected identifier", span)),
        };

        let mut dims = Vec::new();
        while self.try_match(Token::LBrackt) {
            dims.push(self.parse_expr()?);
            self.expect(Token::RBrackt)?;
        }

        let init = if self.try_match(Token::Assign) {
            Some(self.parse_init_val()?)
        } else {
            None
        };

        Ok(VarDef { ident, dims, init, span })
    }

    /// 解析初始值
    /// InitVal -> Expr | '{' (InitVal (',' InitVal)*)? '}'
    fn parse_init_val(&mut self) -> ParseResult<InitVal> {
        if self.try_match(Token::LBrace) {
            let mut vals = Vec::new();
            if !matches!(self.peek(), Some(Token::RBrace)) {
                vals.push(self.parse_init_val()?);
                while self.try_match(Token::Comma) {
                    vals.push(self.parse_init_val()?);
                }
            }
            self.expect(Token::RBrace)?;
            Ok(InitVal::List(vals))
        } else {
            Ok(InitVal::Expr(self.parse_expr()?))
        }
    }

    /// 解析基本类型
    /// BType -> 'int'
    fn parse_btype(&mut self) -> ParseResult<BType> {
        match self.peek() {
            Some(Token::KeywordInt) => {
                self.next();
                Ok(BType::Int)
            }
            Some(Token::KeywordVoid) => {
                self.next();
                Ok(BType::Void)
            }
            _ => Err(ParseError::new("Expected type", self.current_span())),
        }
    }

    // ==================== 函数解析 ====================

    /// 解析函数定义
    /// FuncDef -> BType Ident '(' (FuncParams)? ')' Block
    fn parse_func_def(&mut self) -> ParseResult<FuncDef> {
        let span = self.current_span();
        let return_type = self.parse_btype()?;

        let ident = match self.next() {
            Some((Token::Identifier(id), _, _)) => id,
            _ => return Err(ParseError::new("Expected function name", span)),
        };

        self.expect(Token::LParen)?;
        let params = if matches!(self.peek(), Some(Token::RParen)) {
            Vec::new()
        } else {
            self.parse_func_params()?
        };
        self.expect(Token::RParen)?;

        let block = self.parse_block()?;

        Ok(FuncDef {
            return_type,
            ident,
            params,
            block,
            span,
        })
    }

    /// 解析函数参数列表
    /// FuncParams -> FuncParam (',' FuncParam)*
    fn parse_func_params(&mut self) -> ParseResult<Vec<FuncParam>> {
        let mut params = vec![self.parse_func_param()?];
        while self.try_match(Token::Comma) {
            params.push(self.parse_func_param()?);
        }
        Ok(params)
    }

    /// 解析函数参数
    /// FuncParam -> BType Ident ('[' ']' ('[' Expr ']')*)?
    fn parse_func_param(&mut self) -> ParseResult<FuncParam> {
        let span = self.current_span();
        let btype = self.parse_btype()?;

        let ident = match self.next() {
            Some((Token::Identifier(id), _, _)) => id,
            _ => return Err(ParseError::new("Expected parameter name", span)),
        };

        let mut dims = Vec::new();
        if self.try_match(Token::LBrackt) {
            self.expect(Token::RBrackt)?;
            dims.push(None); // 第一维是 []

            while self.try_match(Token::LBrackt) {
                dims.push(Some(self.parse_expr()?));
                self.expect(Token::RBrackt)?;
            }
        }

        Ok(FuncParam { btype, ident, dims, span })
    }

    // ==================== 语句解析 ====================

    /// 解析语句块
    /// Block -> '{' (BlockItem)* '}'
    fn parse_block(&mut self) -> ParseResult<Block> {
        let span = self.current_span();
        self.expect(Token::LBrace)?;

        let mut items = Vec::new();
        while !matches!(self.peek(), Some(Token::RBrace)) {
            items.push(self.parse_block_item()?);
        }

        self.expect(Token::RBrace)?;
        Ok(Block { items, span })
    }

    /// 解析语句块项
    /// BlockItem -> Decl | Stmt
    fn parse_block_item(&mut self) -> ParseResult<BlockItem> {
        match self.peek() {
            Some(Token::KeywordConst) | Some(Token::KeywordInt) => {
                Ok(BlockItem::Decl(self.parse_decl()?))
            }
            _ => Ok(BlockItem::Stmt(self.parse_stmt()?)),
        }
    }

    /// 解析语句
    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        let span = self.current_span();

        match self.peek() {
            // Block
            Some(Token::LBrace) => Ok(Stmt::Block(self.parse_block()?)),

            // If
            Some(Token::KeywordIf) => {
                self.next();
                self.expect(Token::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(Token::RParen)?;
                let then_stmt = Box::new(self.parse_stmt()?);
                let else_stmt = if self.try_match(Token::KeywordElse) {
                    Some(Box::new(self.parse_stmt()?))
                } else {
                    None
                };
                Ok(Stmt::If {
                    cond,
                    then_stmt,
                    else_stmt,
                    span,
                })
            }

            // While
            Some(Token::KeywordWhile) => {
                self.next();
                self.expect(Token::LParen)?;
                let cond = self.parse_expr()?;
                self.expect(Token::RParen)?;
                let body = Box::new(self.parse_stmt()?);
                Ok(Stmt::While { cond, body, span })
            }

            // Break
            Some(Token::KeywordBreak) => {
                self.next();
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Break { span })
            }

            // Continue
            Some(Token::KeywordContinue) => {
                self.next();
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Continue { span })
            }

            // Return
            Some(Token::KeywordReturn) => {
                self.next();
                let expr = if matches!(self.peek(), Some(Token::Semicolon)) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(Token::Semicolon)?;
                Ok(Stmt::Return { expr, span })
            }

            // Assign or Expr
            _ => {
                // 尝试解析赋值语句或表达式语句
                // 需要向前看以区分 LVal = Expr 和 Expr
                if self.is_assign_stmt() {
                    let lval = self.parse_lval()?;
                    self.expect(Token::Assign)?;
                    let expr = self.parse_expr()?;
                    self.expect(Token::Semicolon)?;
                    Ok(Stmt::Assign { lval, expr, span })
                } else {
                    let expr = if matches!(self.peek(), Some(Token::Semicolon)) {
                        None
                    } else {
                        Some(self.parse_expr()?)
                    };
                    self.expect(Token::Semicolon)?;
                    Ok(Stmt::Expr { expr, span })
                }
            }
        }
    }

    /// 判断是否是赋值语句（需要向前看）
    fn is_assign_stmt(&mut self) -> bool {
        let tokens: Vec<_> = self.tokens.clone().collect();

        // 检查是否以 Ident 开头
        if !matches!(self.peek(), Some(Token::Identifier(_))) {
            self.tokens = tokens.into_iter().peekable();
            return false;
        }
        self.next();

        // 跳过可能的数组索引
        while matches!(self.peek(), Some(Token::LBrackt)) {
            self.next();
            // 跳过索引表达式（简化处理，只跳到 ]）
            let mut depth = 1;
            while depth > 0 {
                match self.peek() {
                    Some(Token::LBrackt) => depth += 1,
                    Some(Token::RBrackt) => depth -= 1,
                    None => break,
                    _ => {}
                }
                self.next();
            }
        }

        // 检查是否是赋值符号
        let is_assign = matches!(self.peek(), Some(Token::Assign));

        // 恢复位置
        self.tokens = tokens.into_iter().peekable();
        is_assign
    }

    // ==================== 表达式解析 ====================

    /// 解析表达式（使用运算符优先级爬升法）
    /// Expr -> LOrExpr
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_binary_expr(0)
    }

    /// 解析二元表达式（运算符优先级爬升）
    fn parse_binary_expr(&mut self, min_prec: u8) -> ParseResult<Expr> {
        let mut left = self.parse_unary_expr()?;

        while let Some(op) = self.peek_binary_op() {
            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            self.next(); // 消费运算符
            let span = self.current_span();

            let next_min_prec = if op.is_left_associative() {
                prec + 1
            } else {
                prec
            };

            let right = self.parse_binary_expr(next_min_prec)?;

            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    /// 查看下一个二元运算符
    fn peek_binary_op(&mut self) -> Option<BinaryOp> {
        match self.peek()? {
            Token::Plus => Some(BinaryOp::Add),
            Token::Minus => Some(BinaryOp::Sub),
            Token::Mult => Some(BinaryOp::Mul),
            Token::Div => Some(BinaryOp::Div),
            Token::Mod => Some(BinaryOp::Mod),
            Token::Lt => Some(BinaryOp::Lt),
            Token::Gt => Some(BinaryOp::Gt),
            Token::Le => Some(BinaryOp::Le),
            Token::Ge => Some(BinaryOp::Ge),
            Token::Eq => Some(BinaryOp::Eq),
            Token::Neq => Some(BinaryOp::Ne),
            Token::And => Some(BinaryOp::And),
            Token::Or => Some(BinaryOp::Or),
            _ => None,
        }
    }

    /// 解析一元表达式
    /// UnaryExpr -> ('+' | '-' | '!') UnaryExpr | PrimaryExpr
    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        let span = self.current_span();

        match self.peek() {
            Some(Token::Plus) => {
                self.next();
                let expr = Box::new(self.parse_unary_expr()?);
                Ok(Expr::Unary {
                    op: UnaryOp::Pos,
                    expr,
                    span,
                })
            }
            Some(Token::Minus) => {
                self.next();
                let expr = Box::new(self.parse_unary_expr()?);
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    expr,
                    span,
                })
            }
            Some(Token::Not) => {
                self.next();
                let expr = Box::new(self.parse_unary_expr()?);
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    expr,
                    span,
                })
            }
            _ => self.parse_primary_expr(),
        }
    }

    /// 解析基本表达式
    /// PrimaryExpr -> '(' Expr ')' | LVal | Number | Call
    fn parse_primary_expr(&mut self) -> ParseResult<Expr> {
        let span = self.current_span();

        match self.peek() {
            // 括号表达式
            Some(Token::LParen) => {
                self.next();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }

            // 数字常量
            Some(Token::IntegerConst(_)) => {
                if let Some((Token::IntegerConst(value), _, _)) = self.next() {
                    Ok(Expr::Number { value, span })
                } else {
                    unreachable!()
                }
            }

            // 标识符（可能是变量、数组或函数调用）
            Some(Token::Identifier(_)) => {
                let tokens: Vec<_> = self.tokens.clone().collect();
                
                // 先尝试解析为左值
                let ident = match self.next() {
                    Some((Token::Identifier(id), _, _)) => id,
                    _ => unreachable!(),
                };

                // 检查是否是函数调用
                if matches!(self.peek(), Some(Token::LParen)) {
                    self.next(); // 消费 '('
                    let args = if matches!(self.peek(), Some(Token::RParen)) {
                        Vec::new()
                    } else {
                        let mut args = vec![self.parse_expr()?];
                        while self.try_match(Token::Comma) {
                            args.push(self.parse_expr()?);
                        }
                        args
                    };
                    self.expect(Token::RParen)?;
                    Ok(Expr::Call { ident, args, span })
                } else {
                    // 恢复并解析为左值
                    self.tokens = tokens.into_iter().peekable();
                    Ok(Expr::LVal(self.parse_lval()?))
                }
            }

            _ => Err(ParseError::new(
                format!("Unexpected token in expression: {:?}", self.peek()),
                span,
            )),
        }
    }

    /// 解析左值
    /// LVal -> Ident ('[' Expr ']')*
    fn parse_lval(&mut self) -> ParseResult<LVal> {
        let span = self.current_span();
        let ident = match self.next() {
            Some((Token::Identifier(id), _, _)) => id,
            _ => return Err(ParseError::new("Expected identifier", span)),
        };

        let mut indices = Vec::new();
        while self.try_match(Token::LBrackt) {
            indices.push(self.parse_expr()?);
            self.expect(Token::RBrackt)?;
        }

        Ok(LVal { ident, indices, span })
    }
}

