// src/tokens.rs
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use std::str::FromStr;

#[derive(Parser)]
#[grammar = "lexer.pest"]
pub struct SysYLexer;

#[derive(Debug, PartialEq, Clone)] // 添加 Clone 派生
pub enum Token {
    KeywordConst,
    KeywordInt,
    KeywordVoid,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    Identifier(String),
    IntegerConst(i64),
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    Assign,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Not,
    And,
    Or,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrackt,
    RBrackt,
    Comma,
    Semicolon,
}

pub fn tokenize(input: &str, line_starts: &[usize]) -> Result<Vec<(Token, String, usize)>, String> {
    // 手动解析输入，逐字符扫描
    let mut tokens = Vec::new();
    let mut pos = 0;
    
    while pos < input.len() {
        // 跳过空白字符
        if let Some(ws_len) = skip_whitespace(&input[pos..]) {
            pos += ws_len;
            continue;
        }
        
        // 跳过注释
        if let Some(comment_len) = skip_comments(&input[pos..]) {
            pos += comment_len;
            continue;
        }
        
        // 尝试匹配各种token
        match try_match_token(&input[pos..]) {
            Some((token, text, len)) => {
                let current_line = get_line_number(line_starts, pos);
                tokens.push((token, text, current_line));
                pos += len;
            }
            None => {
                // 找到错误位置和非法字符
                let error_line = get_line_number(line_starts, pos);
                let illegal_char = input[pos..].chars().next().unwrap_or(' ');
                return Err(format!("Error type A at Line {}: Mysterious character \"{}\".", error_line, illegal_char));
            }
        }
    }
    
    Ok(tokens)
}

// 跳过空白字符
fn skip_whitespace(input: &str) -> Option<usize> {
    let mut len = 0;
    for c in input.chars() {
        if c.is_whitespace() {
            len += c.len_utf8();
        } else {
            break;
        }
    }
    if len > 0 { Some(len) } else { None }
}

// 跳过注释
fn skip_comments(input: &str) -> Option<usize> {
    // 行注释
    if input.starts_with("//") {
        let mut len = 2;
        for c in input[2..].chars() {
            len += c.len_utf8();
            if c == '\n' {
                break;
            }
        }
        return Some(len);
    }
    
    // 块注释
    if input.starts_with("/*") {
        let mut len = 2;
        let mut chars = input[2..].chars();
        let mut prev_char = '\0';
        
        while let Some(c) = chars.next() {
            len += c.len_utf8();
            if prev_char == '*' && c == '/' {
                break;
            }
            prev_char = c;
        }
        return Some(len);
    }
    
    None
}

// 尝试匹配token
fn try_match_token(input: &str) -> Option<(Token, String, usize)> {
    // 关键字
    if let Some((token, len)) = match_keyword(input) {
        return Some((token, input[..len].to_string(), len));
    }
    
    // 标识符
    if let Some(len) = match_identifier(input) {
        let text = input[..len].to_string();
        return Some((Token::Identifier(text.clone()), text, len));
    }
    
    // 数字常量
    if let Some((value, len)) = match_number(input) {
        return Some((Token::IntegerConst(value), value.to_string(), len));
    }
    
    // 运算符和分隔符
    if let Some((token, len)) = match_operator(input) {
        return Some((token, input[..len].to_string(), len));
    }
    
    None
}

// 匹配关键字
fn match_keyword(input: &str) -> Option<(Token, usize)> {
    let keywords = [
        ("const", Token::KeywordConst),
        ("int", Token::KeywordInt),
        ("void", Token::KeywordVoid),
        ("if", Token::KeywordIf),
        ("else", Token::KeywordElse),
        ("while", Token::KeywordWhile),
        ("break", Token::KeywordBreak),
        ("continue", Token::KeywordContinue),
        ("return", Token::KeywordReturn),
    ];
    
    for (kw, token) in keywords.iter() {
        if input.starts_with(kw) {
            // 确保关键字后面不是字母数字或下划线（避免将"intx"识别为"int"）
            if let Some(next_char) = input[kw.len()..].chars().next() {
                if next_char.is_alphanumeric() || next_char == '_' {
                    continue;
                }
            }
            return Some((token.clone(), kw.len())); // 使用 clone() 而不是引用
        }
    }
    
    None
}

// 匹配标识符
fn match_identifier(input: &str) -> Option<usize> {
    let mut len = 0;
    let mut chars = input.chars();
    
    // 第一个字符必须是字母或下划线
    if let Some(c) = chars.next() {
        if c.is_alphabetic() || c == '_' {
            len += c.len_utf8();
        } else {
            return None;
        }
    }
    
    // 后续字符可以是字母、数字或下划线
    for c in chars {
        if c.is_alphanumeric() || c == '_' {
            len += c.len_utf8();
        } else {
            break;
        }
    }
    
    if len > 0 { Some(len) } else { None }
}

// 匹配数字
fn match_number(input: &str) -> Option<(i64, usize)> {
    // 十六进制
    if input.starts_with("0x") || input.starts_with("0X") {
        let mut len = 2;
        let mut value = 0;
        
        for c in input[2..].chars() {
            if let Some(digit) = c.to_digit(16) {
                value = value * 16 + digit as i64;
                len += c.len_utf8();
            } else {
                break;
            }
        }
        
        return if len > 2 { Some((value, len)) } else { None };
    }
    
    // 八进制
    if input.starts_with('0') {
        let mut len = 1;
        let mut value = 0;
        
        for c in input[1..].chars() {
            if let Some(digit) = c.to_digit(8) {
                value = value * 8 + digit as i64;
                len += c.len_utf8();
            } else {
                break;
            }
        }
        
        return if len > 1 { Some((value, len)) } else { Some((0, 1)) };
    }
    
    // 十进制
    let mut len = 0;
    let mut value = 0;
    
    for c in input.chars() {
        if let Some(digit) = c.to_digit(10) {
            value = value * 10 + digit as i64;
            len += c.len_utf8();
        } else {
            break;
        }
    }
    
    if len > 0 { Some((value, len)) } else { None }
}

// 匹配运算符和分隔符
fn match_operator(input: &str) -> Option<(Token, usize)> {
    let operators = [
        ("+", Token::Plus),
        ("-", Token::Minus),
        ("*", Token::Mult),
        ("/", Token::Div),
        ("%", Token::Mod),
        ("=", Token::Assign),
        ("==", Token::Eq),
        ("!=", Token::Neq),
        ("<", Token::Lt),
        (">", Token::Gt),
        ("<=", Token::Le),
        (">=", Token::Ge),
        ("!", Token::Not),
        ("&&", Token::And),
        ("||", Token::Or),
        ("(", Token::LParen),
        (")", Token::RParen),
        ("{", Token::LBrace),
        ("}", Token::RBrace),
        ("[", Token::LBrackt),
        ("]", Token::RBrackt),
        (",", Token::Comma),
        (";", Token::Semicolon),
    ];
    
    for (op, token) in operators.iter() {
        if input.starts_with(op) {
            return Some((token.clone(), op.len())); // 使用 clone() 而不是引用
        }
    }
    
    None
}

fn get_line_number(line_starts: &[usize], pos: usize) -> usize {
    match line_starts.binary_search(&pos) {
        Ok(i) => i + 1,
        Err(i) => i,
    }
}