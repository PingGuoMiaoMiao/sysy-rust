// IR 指令定义
use crate::ir::{VirtualReg, Value, Type};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Instruction {
    Binary {
        result: VirtualReg,
        op: BinaryOp,
        left: Value,
        right: Value,
    },
    Unary {
        result: VirtualReg,
        op: UnaryOp,
        operand: Value,
    },
    Call {
        result: Option<VirtualReg>,
        func: String,
        args: Vec<Value>,
    },
    Alloca {
        result: VirtualReg,
        ty: Type,
    },
    Load {
        result: VirtualReg,
        ptr: Value,
    },
    Store {
        value: Value,
        ptr: Value,
    },
    GetElementPtr {
        result: VirtualReg,
        base: Value,
        indices: Vec<Value>,
    },
    Cast {
        result: VirtualReg,
        value: Value,
        to_ty: Type,
    },
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return(Option<Value>),
    Jump(String),
    Branch {
        cond: Value,
        true_label: String,
        false_label: String,
    },
    Unreachable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Lt, Gt, Le, Ge, Eq, Ne,
    And, Or,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => "add", BinaryOp::Sub => "sub",
            BinaryOp::Mul => "mul", BinaryOp::Div => "div", BinaryOp::Mod => "mod",
            BinaryOp::Lt => "lt", BinaryOp::Gt => "gt",
            BinaryOp::Le => "le", BinaryOp::Ge => "ge",
            BinaryOp::Eq => "eq", BinaryOp::Ne => "ne",
            BinaryOp::And => "and", BinaryOp::Or => "or",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg, Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            UnaryOp::Neg => "neg",
            UnaryOp::Not => "not",
        })
    }
}

