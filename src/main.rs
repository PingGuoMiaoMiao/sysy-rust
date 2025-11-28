use std::{env, fs};

// 词法分析
mod tokens;
mod lexer;   // 新模块化结构

// AST 和 Parser
mod ast;
mod parser;

// 语义和类型检查
mod semantic;
mod type_checker;

// IR 和代码生成
mod ir;
mod ir_gen;
mod ssa;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("使用方法: {} <文件名> [选项]", args[0]);
        eprintln!("选项:");
        eprintln!("  --lex-only    仅执行词法分析");
        eprintln!("  --parse-only  执行到语法分析");
        eprintln!("  --check-only  执行到语义检查");
        std::process::exit(1);
    }

    let filename = &args[1];
    let mode = args.get(2).map(|s| s.as_str()).unwrap_or("--ir");

    let input = fs::read_to_string(filename).expect("读取文件失败");
    let line_starts = build_line_index(&input);

    // 步骤 1: 词法分析
    eprintln!("=== 词法分析 ===");
    let tokens = match tokens::tokenize(&input, &line_starts) {
        Ok(tokens) => {
            eprintln!("词法分析成功，共 {} 个 token", tokens.len());
            if mode == "--lex-only" {
                for (token, text, line) in &tokens {
                    println!("{:?} '{}' at line {}", token, text, line);
                }
                return;
            }
            tokens
        }
        Err(msg) => {
            eprintln!("词法分析错误:\n{}", msg);
            std::process::exit(1);
        }
    };

    // 步骤 2: 语法分析
    eprintln!("\n=== 语法分析 ===");
    let mut parser = parser::Parser::new(tokens);
    let ast = match parser.parse_comp_unit() {
        Ok(ast) => {
            eprintln!("语法分析成功");
            if mode == "--parse-only" {
                println!("{:#?}", ast);
                return;
            }
            ast
        }
        Err(e) => {
            eprintln!("语法分析错误: {} at line {}", e.message, e.span.line);
            std::process::exit(1);
        }
    };

    // 步骤 3: 语义分析
    eprintln!("\n=== 语义分析 ===");
    let mut semantic_analyzer = semantic::SemanticAnalyzer::new();
    if let Err(errors) = semantic_analyzer.analyze(&ast) {
        eprintln!("语义分析错误:");
        for error in errors {
            eprintln!("  {} at line {}", error.message, error.span.line);
        }
        std::process::exit(1);
    }
    eprintln!("语义分析成功");

    // 步骤 4: 类型检查
    eprintln!("\n=== 类型检查 ===");
    let mut type_checker = type_checker::TypeChecker::new();
    match type_checker.check(&ast) {
        Ok(_) => {
            eprintln!("类型检查成功");
            if mode == "--check-only" {
                println!("编译通过（词法、语法、语义、类型检查）");
                eprintln!("\n✓ 所有检查通过");
                return;
            }
        }
        Err(errors) => {
            eprintln!("类型检查错误:");
            for error in errors {
                eprintln!("  {} at line {}", error.message, error.span.line);
            }
            std::process::exit(1);
        }
    }

    // 步骤 5: IR 生成
    eprintln!("\n=== IR 生成 ===");
    let ir_gen = ir_gen::IRGenerator::new();
    let mut ir_module = match ir_gen.generate(&ast) {
        Ok(module) => {
            eprintln!("IR 生成成功");
            module
        }
        Err(e) => {
            eprintln!("IR 生成错误: {}", e);
            std::process::exit(1);
        }
    };

    if mode == "--ir" {
        println!("\n========== 生成的 IR ==========");
        println!("{}", ir_module);
        return;
    }

    // 步骤 6: SSA 构造
    if mode == "--ssa" {
        eprintln!("\n=== SSA 构造 ===");
        match ssa::construct_ssa(&mut ir_module) {
            Ok(_) => {
                eprintln!("SSA 构造成功");
                println!("\n========== SSA 形式的 IR ==========");
                println!("{}", ir_module);
            }
            Err(e) => {
                eprintln!("SSA 构造错误: {}", e);
                std::process::exit(1);
            }
        }
    }

    eprintln!("\n✓ 编译完成");
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