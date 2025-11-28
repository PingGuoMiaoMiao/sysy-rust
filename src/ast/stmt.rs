// 语句
use crate::ast::{Span, Expr, LVal, Decl};

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub items: Vec<BlockItem>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assign {
        lval: LVal,
        expr: Expr,
        span: Span,
    },
    Expr {
        expr: Option<Expr>,
        span: Span,
    },
    Block(Block),
    If {
        cond: Expr,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
        span: Span,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    Break { span: Span },
    Continue { span: Span },
    Return {
        expr: Option<Expr>,
        span: Span,
    },
}

