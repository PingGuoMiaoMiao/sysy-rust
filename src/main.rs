use std::{env, fs};
mod tokens;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }
    let filename = &args[1];
    let input = fs::read_to_string(filename).expect("Failed to read file");

    let line_starts = build_line_index(&input);

    match tokens::tokenize(&input, &line_starts) {
        Ok(tokens) => {
            for (token, text, line) in tokens {
                let token_type = match token {
                    tokens::Token::KeywordConst => "CONST",
                    tokens::Token::KeywordInt => "INT",
                    tokens::Token::KeywordVoid => "VOID",
                    tokens::Token::KeywordIf => "IF",
                    tokens::Token::KeywordElse => "ELSE",
                    tokens::Token::KeywordWhile => "WHILE",
                    tokens::Token::KeywordBreak => "BREAK",
                    tokens::Token::KeywordContinue => "CONTINUE",
                    tokens::Token::KeywordReturn => "RETURN",
                    tokens::Token::Identifier(_) => "IDENT",
                    tokens::Token::IntegerConst(_) => "INTEGER_CONST",
                    tokens::Token::Plus => "PLUS",
                    tokens::Token::Minus => "MINUS",
                    tokens::Token::Mult => "MULT",
                    tokens::Token::Div => "DIV",
                    tokens::Token::Mod => "MOD",
                    tokens::Token::Assign => "ASSIGN",
                    tokens::Token::Eq => "EQ",
                    tokens::Token::Neq => "NEQ",
                    tokens::Token::Lt => "LT",
                    tokens::Token::Gt => "GT",
                    tokens::Token::Le => "LE",
                    tokens::Token::Ge => "GE",
                    tokens::Token::Not => "NOT",
                    tokens::Token::And => "AND",
                    tokens::Token::Or => "OR",
                    tokens::Token::LParen => "L_PAREN",
                    tokens::Token::RParen => "R_PAREN",
                    tokens::Token::LBrace => "L_BRACE",
                    tokens::Token::RBrace => "R_BRACE",
                    tokens::Token::LBrackt => "L_BRACKT",
                    tokens::Token::RBrackt => "R_BRACKT",
                    tokens::Token::Comma => "COMMA",
                    tokens::Token::Semicolon => "SEMICOLON",
                };
                eprintln!("{} {} at Line {}.", token_type, text, line);
            }
        }
        Err(msg) => {
            eprintln!("{}", msg);
            std::process::exit(1);
        }
    }
}

fn build_line_index(input: &str) -> Vec<usize> {
    let mut line_starts = vec![0];
    for (i, c) in input.char_indices() {
        if c == '\n' {
            line_starts.push(i + 1);
        }
    }
    line_starts
}