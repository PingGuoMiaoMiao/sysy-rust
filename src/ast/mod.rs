// AST 模块
pub mod types;
pub mod expr;
pub mod stmt;
pub mod decl;

pub use types::*;
pub use expr::*;
pub use stmt::*;
pub use decl::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(line: usize, column: usize) -> Self {
        Span { line, column }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompUnit {
    pub items: Vec<CompUnitItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompUnitItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

