// Token 定义
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "lexer.pest"]
pub struct SysYLexer;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // 关键字
    KeywordConst,
    KeywordInt,
    KeywordVoid,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    
    // 标识符和常量
    Identifier(String),
    IntegerConst(i64),
    
    // 运算符
    Plus, Minus, Mult, Div, Mod,
    Assign, Eq, Neq, Lt, Gt, Le, Ge,
    Not, And, Or,
    
    // 分隔符
    LParen, RParen,
    LBrace, RBrace,
    LBrackt, RBrackt,
    Comma, Semicolon,
}

