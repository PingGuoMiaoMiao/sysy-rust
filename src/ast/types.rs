// 类型定义
use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum BType {
    Int,
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub btype: BType,
    pub ident: String,
    pub dims: Vec<Option<crate::ast::Expr>>,
    pub span: Span,
}

