// 声明
use crate::ast::{Span, BType, Expr, FuncParam, Block};

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Const(ConstDecl),
    Var(VarDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub btype: BType,
    pub defs: Vec<ConstDef>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDef {
    pub ident: String,
    pub dims: Vec<Expr>,
    pub init: ConstInitVal,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstInitVal {
    Expr(Expr),
    List(Vec<ConstInitVal>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub btype: BType,
    pub defs: Vec<VarDef>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDef {
    pub ident: String,
    pub dims: Vec<Expr>,
    pub init: Option<InitVal>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InitVal {
    Expr(Expr),
    List(Vec<InitVal>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub return_type: BType,
    pub ident: String,
    pub params: Vec<FuncParam>,
    pub block: Block,
    pub span: Span,
}

