use std::collections::HashMap;
use std::env;
use std::fs;
use std::process::Command;
use std::time::Instant;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Let,
    Stamp,
    Dump,
    Disguise,
    Identifier,
    StringLiteral,
    NumberLiteral,
    Equals,
    PlusEquals,
    Colon,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    GreaterThan,
    Comma,
    If,
    Else,
    Loop,
    // New tokens for your updated layout:
    Pub,
    Struct,
    ChangeableModifier, // For *c
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct AholaValue {
    pub data: Vec<String>,
    pub var_type: String,
    pub changeable: bool, // Track if it was declared with *c
}

fn detects_pure_rust(code: &str) -> bool {
    code.contains("fn main")
        || code.contains("println!")
        || code.contains("let mut")
        || code.contains("use std::")
}

fn evaluate_math_func(func_name: &str, args_str: &str) -> String {
    let parts: Vec<f64> = args_str
        .split(',')
        .map(|s| s.trim().parse::<f64>().unwrap_or(0.0))
        .collect();

    match func_name {
        "min" => {
            if parts.is_empty() { return "0".to_string(); }
            let mut current_min = parts[0];
            for &val in &parts {
                if val < current_min { current_min = val; }
            }
            current_min.to_string()
        }
        "mid" => {
            if parts.len() != 3 { return "0".to_string(); }
            let mut sorted = parts.clone();
            sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
            sorted[1].to_string()
        }
        "max" => {
            if parts.is_empty() { return "0".to_string(); }
            let mut current_max = parts[0];
            for &val in &parts {
                if val > current_max { current_max = val; }
            }
            current_max.to_string()
        }
        "round_up" => {
            if parts.is_empty() { return "0".to_string(); }
            parts[0].ceil().to_string()
        }
        "round_down" => {
            if parts.is_empty() { return "0".to_string(); }
            parts[0].floor().to_string()
        }
        "abs" => {
            if parts.is_empty() { return "0".to_string(); }
            parts[0].abs().to_string()
        }
        _ => "0".to_string(),
    }
}

fn lex(code: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let lines = code.lines();

    for line in lines {
        let clean_line = if let Some(idx) = line.find("//") {
            &line[..idx]
        } else {
            line
        };
        let mut chars = clean_line.chars().peekable();

        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                chars.next();
            } else if ch == '"' {
                chars.next();
                let mut string_lit = String::new();
                while let Some(&str_ch) = chars.peek() {
                    if str_ch == '"' {
                        chars.next();
                        break;
                    }
                    string_lit.push(chars.next().unwrap());
                }
                tokens.push(Token {
                    token_type: TokenType::StringLiteral,
                    value: string_lit,
                });
            } else if ch == '*' {
                chars.next();
                if chars.peek() == Some(&'c') {
                    chars.next();
                    tokens.push(Token {
                        token_type: TokenType::ChangeableModifier,
                        value: "*c".to_string(),
                    });
                } else {
                    return Err("Expected 'c' after '*' for changeable modifier".to_string());
                }
            } else if ch == ':' {
                tokens.push(Token { token_type: TokenType::Colon, value: ch.to_string() });
                chars.next();
            } else if ch == '[' {
                tokens.push(Token { token_type: TokenType::LeftBracket, value: ch.to_string() });
                chars.next();
            } else if ch == ']' {
                tokens.push(Token { token_type: TokenType::RightBracket, value: ch.to_string() });
                chars.next();
            } else if ch == '{' {
                tokens.push(Token { token_type: TokenType::LeftBrace, value: ch.to_string() });
                chars.next();
            } else if ch == '}' {
                tokens.push(Token { token_type: TokenType::RightBrace, value: ch.to_string() });
                chars.next();
            } else if ch == '>' {
                tokens.push(Token { token_type: TokenType::GreaterThan, value: ch.to_string() });
                chars.next();
            } else if ch == ',' {
                tokens.push(Token { token_type: TokenType::Comma, value: ch.to_string() });
                chars.next();
            } else if ch == '=' {
                tokens.push(Token { token_type: TokenType::Equals, value: ch.to_string() });
                chars.next();
            } else if ch == '+' {
                chars.next();
                if chars.peek() == Some(&'=') {
                    tokens.push(Token {
                        token_type: TokenType::PlusEquals,
                        value: "+=".to_string(),
                    });
                    chars.next();
                } else {
                    return Err("Unsupported operator '+' without '='".to_string());
                }
            } else {
                let mut word = String::new();
                while let Some(&word_ch) = chars.peek() {
                    if word_ch.is_whitespace()
                        || vec!['"', ':', '[', ']', '{', '}', ',', '=', '+', '>', '(', ')']
                            .contains(&word_ch)
                    {
                        break;
                    }
                    word.push(chars.next().unwrap());
                }

                if chars.peek() == Some(&'(') {
                    chars.next();
                    let mut args_inner = String::new();
                    while let Some(&arg_ch) = chars.peek() {
                        if arg_ch == ')' {
                            chars.next();
                            break;
                        }
                        args_inner.push(chars.next().unwrap());
                    }
                    let calculated_result = evaluate_math_func(&word, &args_inner);
                    tokens.push(Token {
                        token_type: TokenType::NumberLiteral,
                        value: calculated_result,
                    });
                    continue;
                }

                match word.as_str() {
                    "let" => tokens.push(Token { token_type: TokenType::Let, value: word }),
                    "pub" => tokens.push(Token { token_type: TokenType::Pub, value: word }),
                    "struct" => tokens.push(Token { token_type: TokenType::Struct, value: word }),
                    "stamp" => tokens.push(Token { token_type: TokenType::Stamp, value: word }),
                    "dump" => tokens.push(Token { token_type: TokenType::Dump, value: word }),
                    "disguise" => tokens.push(Token { token_type: TokenType::Disguise, value: word }),
                    "if" => tokens.push(Token { token_type: TokenType::If, value: word }),
                    "else" => tokens.push(Token { token_type: TokenType::Else, value: word }),
                    "loop" => tokens.push(Token { token_type: TokenType::Loop, value: word }),
                    _ => {
                        if word.is_empty() { continue; }
                        if word.chars().all(|c| c.is_numeric() || c == '.' || c == '-') {
                            tokens.push(Token {
                                token_type: TokenType::NumberLiteral,
                                value: word,
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::Identifier,
                                value: word,
                            });
                        }
                    }
                }
            }
        }
    }
    Ok(tokens)
}

fn compile_and_run_rust_fallback(code: &str) {
    let bold_hex_dark_green = "\x1b[1;38;2;30;104;35m";
    let reset_color = "\x1b[0m";
    println!(
        "{}Ahola Engine: Undefined dialect detected. Testing fallback to Rust toolchain...{}",
        bold_hex_dark_green, reset_color
    );
    let temp_src = "temp_fallback.rs";
    let temp_bin = "./temp_fallback_bin";
    fs::write(temp_src, code).expect("Failed to create temporary compilation asset.");
    let compile_status = Command::new("rustc")
        .args(&[temp_src, "-o", temp_bin])
        .status();
    match compile_status {
        Ok(status) if status.success() => {
            let output = Command::new(temp_bin)
                .output()
                .expect("Failed to execute compiled Rust binary.");
            println!("{}", String::from_utf8_lossy(&output.stdout));
            let _ = fs::remove_file(temp_src);
            let _ = fs::remove_file(temp_bin);
        }
        _ => {
            println!(
                "\x1b[1;31mCompilation Error:\x1b[0m Code failed both native Ahola parsing and the fallback Rust validation."
            );
            let _ = fs::remove_file(temp_src);
        }
    }
}

fn interpret_tokens(tokens: &[Token], variables: &mut HashMap<String, AholaValue>, outputs: &mut Vec<String>) {
    let mut idx = 0;
    while idx < tokens.len() {
        match tokens[idx].token_type {
            TokenType::Let | TokenType::Disguise => {
                let mut is_changeable = false;
                let mut current_idx = idx + 1;

                if current_idx < tokens.len() && tokens[current_idx].token_type == TokenType::Identifier {
                    let var_name = tokens[current_idx].value.clone();
                    current_idx += 1;

                    if current_idx < tokens.len() && tokens[current_idx].token_type == TokenType::ChangeableModifier {
                        is_changeable = true;
                        current_idx += 1;
                    }

                    let mut current_type = "deduced".to_string();
                    if current_idx < tokens.len() && tokens[current_idx].token_type == TokenType::Colon {
                        current_type = tokens[current_idx + 1].value.clone();
                        current_idx += 2;
                    }

                    if current_idx < tokens.len() && tokens[current_idx].token_type == TokenType::Equals {
                        let value_idx = current_idx + 1;
                        if value_idx < tokens.len() {
                            if tokens[value_idx].token_type == TokenType::LeftBracket {
                                let mut card_items = Vec::new();
                                let mut scan = value_idx + 1;
                                while scan < tokens.len() && tokens[scan].token_type != TokenType::RightBracket {
                                    if tokens[scan].token_type == TokenType::StringLiteral || tokens[scan].token_type == TokenType::NumberLiteral {
                                        card_items.push(tokens[scan].value.clone());
                                    }
                                    scan += 1;
                                }
                                variables.insert(var_name, AholaValue {
                                    data: card_items,
                                    var_type: "card".to_string(),
                                    changeable: is_changeable,
                                });
                                idx = scan + 1;
                                continue;
                            } else {
                                let var_val = tokens[value_idx].value.clone();
                                if current_type == "deduced" {
                                    current_type = if tokens[value_idx].token_type == TokenType::NumberLiteral {
                                        "int/float".to_string()
                                    } else {
                                        "string".to_string()
                                    };
                                }
                                variables.insert(var_name, AholaValue {
                                    data: vec![var_val],
                                    var_type: current_type,
                                    changeable: is_changeable,
                                });
                                idx = value_idx + 1;
                                continue;
                            }
                        }
                    }
                }
                idx += 1;
            }
            TokenType::Identifier => {
                let var_name = tokens[idx].value.clone();
                if idx + 1 < tokens.len() && tokens[idx + 1].token_type == TokenType::PlusEquals {
                    let modifier_idx = idx + 2;
                    if modifier_idx < tokens.len() {
                        let modifier = tokens[modifier_idx].value.clone();
                        if let Some(existing) = variables.get_mut(&var_name) {
                            if !existing.changeable && tokens[idx - 1].token_type != TokenType::Let {
                                println!("\x1b[1;31mRuntime Error:\x1b[0m Cannot modify immutable variable '{}'. Did you forget *c?", var_name);
                                std::process::exit(1);
                            }
                            if existing.var_type == "card" {
                                existing.data.push(modifier);
                            } else if existing.var_type == "string" {
                                existing.data[0] = format!("{}{}", existing.data[0], modifier);
                            } else {
                                let old_num: f64 = existing.data[0].parse().unwrap_or(0.0);
                                let mod_num: f64 = modifier.parse().unwrap_or(0.0);
                                existing.data[0] = (old_num + mod_num).to_string();
                            }
                        }
                        idx = modifier_idx + 1;
                        continue;
                    }
                }
                idx += 1;
            }
            TokenType::Pub => {
                // Look ahead for struct @main { ... }
                if idx + 3 < tokens.len() 
                    && tokens[idx + 1].token_type == TokenType::Struct 
                    && tokens[idx + 2].value == "@main" 
                    && tokens[idx + 3].token_type == TokenType::LeftBrace 
                {
                    let mut inner_tokens = Vec::new();
                    let mut scan = idx + 4;
                    let mut brace_count = 1;
                    
                    while scan < tokens.len() && brace_count > 0 {
                        if tokens[scan].token_type == TokenType::LeftBrace { brace_count += 1; }
                        if tokens[scan].token_type == TokenType::RightBrace { brace_count -= 1; }
                        if brace_count > 0 {
                            inner_tokens.push(tokens[scan].clone());
                        }
                        scan += 1;
                    }
                    // Run the code block safely inside the entry scope!
                    interpret_tokens(&inner_tokens, variables, outputs);
                    idx = scan;
                } else {
                    idx += 1;
                }
            }
            TokenType::Stamp => {
                if idx + 1 < tokens.len() {
                    let mut message = tokens[idx + 1].value.clone();
                    for (var_name, var_obj) in variables.iter() {
                        let dynamic_pattern = format!("\\({})", var_name);
                        if message.contains(&dynamic_pattern) {
                            message = message.replace(&dynamic_pattern, &var_obj.data.join(", "));
                        }
                    }
                    outputs.push(message);
                    idx += 2;
                } else {
                    idx += 1;
                }
            }
            TokenType::Dump => {
                if idx + 1 < tokens.len() {
                    let target = &tokens[idx + 1].value;
                    if let Some(var) = variables.get(target) {
                        outputs.push(format!("{:?}", var.data));
                    }
                    idx += 2;
                } else {
                    idx += 1;
                }
            }
            _ => idx += 1,
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 4 || args[1] != "yeah" || args[2] != "run" {
        println!("Usage: ./engine yeah run <filename>");
        return;
    }
    let file_path = &args[3];
    let start_time = Instant::now();
    let bold_hex_dark_green = "\x1b[1;38;2;30;104;35m";
    let reset_color = "\x1b[0m";

    let ahola_code = fs::read_to_string(file_path).expect("Failed to read script.");

    if detects_pure_rust(&ahola_code) {
        compile_and_run_rust_fallback(&ahola_code);
        return;
    }

    let tokens = match lex(&ahola_code) {
        Ok(t) => t,
        Err(_) => {
            compile_and_run_rust_fallback(&ahola_code);
            return;
        }
    };

    println!("{}Compiling{} {}", bold_hex_dark_green, reset_color, file_path);
    
    let mut variables: HashMap<String, AholaValue> = HashMap::new();
    let mut outputs = Vec::new();

    interpret_tokens(&tokens, &mut variables, &mut outputs);

    let duration = start_time.elapsed().as_millis();
    println!("{}Compiled{} {} in {}ms", bold_hex_dark_green, reset_color, file_path, duration);
    
    for out in outputs {
        println!("{}", out);
    }
}
