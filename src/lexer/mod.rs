// 词法分析模块
pub mod token;

use token::Token;

pub fn tokenize(input: &str, line_starts: &[usize]) -> Result<Vec<(Token, String, usize)>, String> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut pos = 0;
    
    while pos < input.len() {
        if let Some(ws_len) = skip_whitespace(&input[pos..]) {
            pos += ws_len;
            continue;
        }
        
        if let Some(comment_len) = skip_comments(&input[pos..]) {
            pos += comment_len;
            continue;
        }
        
        match try_match_token(&input[pos..]) {
            Some((token, text, len)) => {
                let line = get_line_number(line_starts, pos);
                tokens.push((token, text, line));
                pos += len;
            }
            None => {
                let line = get_line_number(line_starts, pos);
                if let Some(c) = input[pos..].chars().next() {
                    errors.push(format!("Error type A at Line {}: Mysterious character \"{}\".", line, c));
                    pos += c.len_utf8();
                } else {
                    break;
                }
            }
        }
    }
    
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors.join("\n"))
    }
}

fn skip_whitespace(input: &str) -> Option<usize> {
    let len = input.chars().take_while(|c| c.is_whitespace()).map(|c| c.len_utf8()).sum();
    if len > 0 { Some(len) } else { None }
}

fn skip_comments(input: &str) -> Option<usize> {
    if input.starts_with("//") {
        return Some(input.find('\n').map(|i| i + 1).unwrap_or(input.len()));
    }
    
    if input.starts_with("/*") {
        let mut end = 2;
        let mut prev = '\0';
        for c in input[2..].chars() {
            end += c.len_utf8();
            if prev == '*' && c == '/' {
                break;
            }
            prev = c;
        }
        return Some(end);
    }
    
    None
}

fn try_match_token(input: &str) -> Option<(Token, String, usize)> {
    if let Some((token, len)) = match_keyword(input) {
        return Some((token, input[..len].to_string(), len));
    }
    
    if let Some(len) = match_identifier(input) {
        let text = input[..len].to_string();
        return Some((Token::Identifier(text.clone()), text, len));
    }
    
    if let Some((value, len)) = match_number(input) {
        return Some((Token::IntegerConst(value), value.to_string(), len));
    }
    
    if let Some((token, len)) = match_operator(input) {
        return Some((token, input[..len].to_string(), len));
    }
    
    None
}

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
            let next = input[kw.len()..].chars().next();
            if next.map_or(true, |c| !c.is_alphanumeric() && c != '_') {
                return Some((token.clone(), kw.len()));
            }
        }
    }
    None
}

fn match_identifier(input: &str) -> Option<usize> {
    let mut chars = input.chars();
    let first = chars.next()?;
    
    if !first.is_alphabetic() && first != '_' {
        return None;
    }
    
    let mut len = first.len_utf8();
    for c in chars {
        if c.is_alphanumeric() || c == '_' {
            len += c.len_utf8();
        } else {
            break;
        }
    }
    Some(len)
}

fn match_number(input: &str) -> Option<(i64, usize)> {
    if input.starts_with("0x") || input.starts_with("0X") {
        let mut len = 2;
        let mut val = 0i64;
        for c in input[2..].chars() {
            if let Some(d) = c.to_digit(16) {
                val = val * 16 + d as i64;
                len += 1;
            } else {
                break;
            }
        }
        return if len > 2 { Some((val, len)) } else { None };
    }
    
    if input.starts_with('0') && input.len() > 1 {
        let mut len = 1;
        let mut val = 0i64;
        for c in input[1..].chars() {
            if let Some(d) = c.to_digit(8) {
                val = val * 8 + d as i64;
                len += 1;
            } else {
                break;
            }
        }
        return if len > 1 { Some((val, len)) } else { Some((0, 1)) };
    }
    
    let mut len = 0;
    let mut val = 0i64;
    for c in input.chars() {
        if let Some(d) = c.to_digit(10) {
            val = val * 10 + d as i64;
            len += 1;
        } else {
            break;
        }
    }
    if len > 0 { Some((val, len)) } else { None }
}

fn match_operator(input: &str) -> Option<(Token, usize)> {
    let ops = [
        ("==", Token::Eq), ("!=", Token::Neq),
        ("<=", Token::Le), (">=", Token::Ge),
        ("&&", Token::And), ("||", Token::Or),
        ("+", Token::Plus), ("-", Token::Minus),
        ("*", Token::Mult), ("/", Token::Div),
        ("%", Token::Mod), ("=", Token::Assign),
        ("<", Token::Lt), (">", Token::Gt),
        ("!", Token::Not), ("(", Token::LParen),
        (")", Token::RParen), ("{", Token::LBrace),
        ("}", Token::RBrace), ("[", Token::LBrackt),
        ("]", Token::RBrackt), (",", Token::Comma),
        (";", Token::Semicolon),
    ];
    
    for (op, token) in ops.iter() {
        if input.starts_with(op) {
            return Some((token.clone(), op.len()));
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

