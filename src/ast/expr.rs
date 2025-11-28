// 表达式
use crate::ast::Span;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Call {
        ident: String,
        args: Vec<Expr>,
        span: Span,
    },
    LVal(LVal),
    Number {
        value: i64,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct LVal {
    pub ident: String,
    pub indices: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Lt, Gt, Le, Ge, Eq, Ne,
    And, Or,
}

impl BinaryOp {
    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOp::Or => 1,
            BinaryOp::And => 2,
            BinaryOp::Eq | BinaryOp::Ne => 3,
            BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => 4,
            BinaryOp::Add | BinaryOp::Sub => 5,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 6,
        }
    }
    
    pub fn is_left_associative(&self) -> bool {
        true
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => "+", BinaryOp::Sub => "-",
            BinaryOp::Mul => "*", BinaryOp::Div => "/", BinaryOp::Mod => "%",
            BinaryOp::Lt => "<", BinaryOp::Gt => ">",
            BinaryOp::Le => "<=", BinaryOp::Ge => ">=",
            BinaryOp::Eq => "==", BinaryOp::Ne => "!=",
            BinaryOp::And => "&&", BinaryOp::Or => "||",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Pos, Neg, Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            UnaryOp::Pos => "+",
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        })
    }
}

