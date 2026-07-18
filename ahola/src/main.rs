use num_bigint::{BigInt, BigUint, ToBigInt};
use num_traits::{One, Zero};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::process::Command;
use std::time::Instant;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Let,
    Type,
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
    While,
    Fn,
    Return,
    ReadFile,
    Embed,
    CallRust,
    FileHook,
    Pub,
    Struct,
    ChangeableModifier,
    Dot,
    Minus,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AholaData {
    I4M(BigInt),   // Signed 4 Million bit integer space
    U4M(BigUint),  // Unsigned 4 Million bit integer space
    Int5M(BigInt), // Signed 5 Million bit massive arbitrary limit
    Float(f64),
    String(String),
    DbRef(String), // Reference pointing to a `.db` layer
    Card(Vec<AholaData>),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Store(usize, AholaData),
    PlusEqual(usize, AholaData),
    Stamp(String),
    Dump(usize),
    Jump(usize),
    JumpIfFalse(usize),
    ReadFile(usize, usize),
    InlineRust(String),
    CallRustFunc {
        func_name: String,
        arg: String,
    },
    ExternalFileHook(String),
    Call(usize),
    Return,
    // Native Database Operation with 4M-5M Bit Width range compatibility
    DbRangeRequest {
        target_slot: Option<usize>, // None if running standalone
        db_var_slot: usize,
        start: BigUint,
        end: BigUint,
    },
    Ban(usize, String),
}

pub struct Symbol {
    pub slot: usize,
    pub var_type: String,
    pub changeable: bool,
    pub is_private: bool,
}

pub struct SymbolTable {
    pub symbols: HashMap<String, Symbol>,
    pub next_slot: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: HashMap::new(),
            next_slot: 0,
        }
    }
    pub fn register(
        &mut self,
        name: &str,
        var_type: String,
        changeable: bool,
        is_private: bool,
    ) -> usize {
        let slot = self.next_slot;
        self.symbols.insert(
            name.to_string(),
            Symbol {
                slot,
                var_type,
                changeable,
                is_private,
            },
        );
        self.next_slot += 1;
        slot
    }
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
            if parts.is_empty() {
                return "0".to_string();
            }
            let mut current_min = parts[0];
            for &val in &parts {
                if val < current_min {
                    current_min = val;
                }
            }
            current_min.to_string()
        }
        "mid" => {
            if parts.len() != 3 {
                return "0".to_string();
            }
            let mut sorted = parts.clone();
            sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
            sorted[1].to_string()
        }
        "max" => {
            if parts.is_empty() {
                return "0".to_string();
            }
            let mut current_max = parts[0];
            for &val in &parts {
                if val > current_max {
                    current_max = val;
                }
            }
            current_max.to_string()
        }
        "round_up" => {
            if parts.is_empty() {
                "0".to_string()
            } else {
                parts[0].ceil().to_string()
            }
        }
        "round_down" => {
            if parts.is_empty() {
                "0".to_string()
            } else {
                parts[0].floor().to_string()
            }
        }
        "abs" => {
            if parts.is_empty() {
                "0".to_string()
            } else {
                parts[0].abs().to_string()
            }
        }
        _ => "0".to_string(),
    }
}

pub fn lex(code: &str) -> Result<Vec<Token>, String> {
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
            } else if ch == '.' {
                tokens.push(Token {
                    token_type: TokenType::Dot,
                    value: ".".to_string(),
                });
                chars.next();
            } else if ch == '-' {
                tokens.push(Token {
                    token_type: TokenType::Minus,
                    value: "-".to_string(),
                });
                chars.next();
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
                tokens.push(Token {
                    token_type: TokenType::Colon,
                    value: ch.to_string(),
                });
                chars.next();
            } else if ch == '[' {
                tokens.push(Token {
                    token_type: TokenType::LeftBracket,
                    value: ch.to_string(),
                });
                chars.next();
            } else if ch == ']' {
                tokens.push(Token {
                    token_type: TokenType::RightBracket,
                    value: ch.to_string(),
                });
                chars.next();
            } else if ch == '{' {
                tokens.push(Token {
                    token_type: TokenType::LeftBrace,
                    value: ch.to_string(),
                });
                chars.next();
            } else if ch == '}' {
                tokens.push(Token {
                    token_type: TokenType::RightBrace,
                    value: ch.to_string(),
                });
                chars.next();
            } else if ch == '>' {
                tokens.push(Token {
                    token_type: TokenType::GreaterThan,
                    value: ch.to_string(),
                });
                chars.next();
            } else if ch == ',' {
                tokens.push(Token {
                    token_type: TokenType::Comma,
                    value: ch.to_string(),
                });
                chars.next();
            } else if ch == '=' {
                tokens.push(Token {
                    token_type: TokenType::Equals,
                    value: ch.to_string(),
                });
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
                        || vec![
                            '"', ':', '[', ']', '{', '}', ',', '=', '+', '>', '(', ')', '.', '-',
                        ]
                        .contains(&word_ch)
                    {
                        break;
                    }
                    word.push(chars.next().unwrap());
                }

                if chars.peek() == Some(&'c') && word.is_empty() {
                    chars.next();
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

                if word.starts_with("call.") && word.ends_with(".rs") {
                    tokens.push(Token {
                        token_type: TokenType::FileHook,
                        value: word,
                    });
                } else {
                    match word.as_str() {
                        "let" => tokens.push(Token {
                            token_type: TokenType::Let,
                            value: word,
                        }),
                        "type" => tokens.push(Token {
                            token_type: TokenType::Type,
                            value: word,
                        }),
                        "pub" => tokens.push(Token {
                            token_type: TokenType::Pub,
                            value: word,
                        }),
                        "struct" => tokens.push(Token {
                            token_type: TokenType::Struct,
                            value: word,
                        }),
                        "stamp" => tokens.push(Token {
                            token_type: TokenType::Stamp,
                            value: word,
                        }),
                        "dump" => tokens.push(Token {
                            token_type: TokenType::Dump,
                            value: word,
                        }),
                        "disguise" => tokens.push(Token {
                            token_type: TokenType::Disguise,
                            value: word,
                        }),
                        "if" => tokens.push(Token {
                            token_type: TokenType::If,
                            value: word,
                        }),
                        "else" => tokens.push(Token {
                            token_type: TokenType::Else,
                            value: word,
                        }),
                        "loop" => tokens.push(Token {
                            token_type: TokenType::Loop,
                            value: word,
                        }),
                        "while" => tokens.push(Token {
                            token_type: TokenType::While,
                            value: word,
                        }),
                        "fn" => tokens.push(Token {
                            token_type: TokenType::Fn,
                            value: word,
                        }),
                        "return" => tokens.push(Token {
                            token_type: TokenType::Return,
                            value: word,
                        }),
                        "read_file" => tokens.push(Token {
                            token_type: TokenType::ReadFile,
                            value: word,
                        }),
                        "embed" => tokens.push(Token {
                            token_type: TokenType::Embed,
                            value: word,
                        }),
                        "call_rust" => tokens.push(Token {
                            token_type: TokenType::CallRust,
                            value: word,
                        }),
                        _ => {
                            if word.is_empty() {
                                continue;
                            }
                            if word.chars().all(|c| c.is_numeric()) {
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
    }
    Ok(tokens)
}

fn compile_and_run_rust_fallback(code: &str) {
    let bold_hex_dark_green = "\x1b[1;38;2;30;104;35m";
    let reset_color = "\x1b[0m";
    println!(
        "{}Ahola Engine: Fallback trigger evaluation activating...{}",
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
                "\x1b[1;31mCompilation Error:\x1b[0m Syntax failed evaluation criteria check."
            );
            let _ = fs::remove_file(temp_src);
        }
    }
}

pub fn compile_tokens(
    tokens: &[Token],
    symbol_table: &mut SymbolTable,
    bytecode: &mut Vec<OpCode>,
) {
    let mut idx = 0;
    while idx < tokens.len() {
        // Lookahead parsing logic to capture clean variable assignments or pure requests
        if tokens[idx].token_type == TokenType::Identifier
            && idx + 1 < tokens.len()
            && tokens[idx + 1].token_type == TokenType::Equals
        {
            let target_var = tokens[idx].value.clone();
            let mut scan = idx + 2;

            if scan < tokens.len() && tokens[scan].token_type == TokenType::Dot {
                // Example: database instantiation logic -> var = .db
                if scan + 1 < tokens.len() && tokens[scan + 1].value == "db" {
                    let slot = symbol_table.register(&target_var, "db".to_string(), true, false);
                    bytecode.push(OpCode::Store(slot, AholaData::DbRef(target_var.clone())));
                    idx = scan + 2;
                    continue;
                }
            }

            // Example check: capture variable assignment requests -> user = users.request(1-100)
            if scan + 2 < tokens.len()
                && tokens[scan].token_type == TokenType::Identifier
                && tokens[scan + 1].token_type == TokenType::Dot
                && tokens[scan + 2].value == "request"
            {
                let db_source = tokens[scan].value.clone();
                if let Some(db_sym) = symbol_table.symbols.get(&db_source) {
                    let db_slot = db_sym.slot;
                    scan += 3; // Move into the request boundaries
                    if scan < tokens.len() && tokens[scan].token_type == TokenType::LeftBracket {
                        scan += 1;
                        let mut start_str = String::new();
                        while scan < tokens.len()
                            && tokens[scan].token_type == TokenType::NumberLiteral
                        {
                            start_str.push_str(&tokens[scan].value);
                            scan += 1;
                        }
                        if scan < tokens.len() && tokens[scan].token_type == TokenType::Minus {
                            scan += 1;
                            let mut end_str = String::new();
                            while scan < tokens.len()
                                && tokens[scan].token_type == TokenType::NumberLiteral
                            {
                                end_str.push_str(&tokens[scan].value);
                                scan += 1;
                            }
                            if scan < tokens.len()
                                && tokens[scan].token_type == TokenType::RightBracket
                            {
                                let start_big =
                                    start_str.parse::<BigUint>().unwrap_or(BigUint::zero());
                                let end_big = end_str.parse::<BigUint>().unwrap_or(BigUint::zero());

                                let target_slot = symbol_table.register(
                                    &target_var,
                                    "card".to_string(),
                                    true,
                                    false,
                                );
                                bytecode.push(OpCode::DbRangeRequest {
                                    target_slot: Some(target_slot),
                                    db_var_slot: db_slot,
                                    start: start_big,
                                    end: end_big,
                                });
                                idx = scan + 1;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        // Handle pure standalone calls (e.g. users.request(1-100))
        if tokens[idx].token_type == TokenType::Identifier
            && idx + 2 < tokens.len()
            && tokens[idx + 1].token_type == TokenType::Dot
            && tokens[idx + 2].value == "request"
        {
            let db_source = tokens[idx].value.clone();
            if let Some(db_sym) = symbol_table.symbols.get(&db_source) {
                let db_slot = db_sym.slot;
                let mut scan = idx + 3;
                if scan < tokens.len() && tokens[scan].token_type == TokenType::LeftBracket {
                    scan += 1;
                    let mut start_str = String::new();
                    while scan < tokens.len() && tokens[scan].token_type == TokenType::NumberLiteral
                    {
                        start_str.push_str(&tokens[scan].value);
                        scan += 1;
                    }
                    if scan < tokens.len() && tokens[scan].token_type == TokenType::Minus {
                        scan += 1;
                        let mut end_str = String::new();
                        while scan < tokens.len()
                            && tokens[scan].token_type == TokenType::NumberLiteral
                        {
                            end_str.push_str(&tokens[scan].value);
                            scan += 1;
                        }
                        if scan < tokens.len() && tokens[scan].token_type == TokenType::RightBracket
                        {
                            let start_big = start_str.parse::<BigUint>().unwrap_or(BigUint::zero());
                            let end_big = end_str.parse::<BigUint>().unwrap_or(BigUint::zero());

                            bytecode.push(OpCode::DbRangeRequest {
                                target_slot: None,
                                db_var_slot: db_slot,
                                start: start_big,
                                end: end_big,
                            });
                            idx = scan + 1;
                            continue;
                        }
                    }
                }
            }
        }

        // Handle direct memory destructive .ban commands
        if tokens[idx].token_type == TokenType::Dot
            && idx + 1 < tokens.len()
            && tokens[idx + 1].value == "ban"
            && idx + 2 < tokens.len()
        {
            let target_banned = tokens[idx + 2].value.clone();
            if let Some(sym) = symbol_table.symbols.get(&target_banned) {
                bytecode.push(OpCode::Ban(sym.slot, target_banned.clone()));
            }
            idx += 3;
            continue;
        }

        // General compiler execution match branch
        match tokens[idx].token_type {
            TokenType::Let | TokenType::Type | TokenType::Disguise | TokenType::Identifier => {
                let mut current_idx = idx;
                let mut is_changeable = false;

                if tokens[current_idx].token_type == TokenType::Let
                    || tokens[current_idx].token_type == TokenType::Type
                    || tokens[current_idx].token_type == TokenType::Disguise
                {
                    current_idx += 1;
                }

                if current_idx < tokens.len()
                    && tokens[current_idx].token_type == TokenType::Identifier
                {
                    let var_name = tokens[current_idx].value.clone();
                    let is_constant = var_name.chars().next().map_or(false, |c| c.is_uppercase());
                    if is_constant {
                        is_changeable = false;
                    }

                    if current_idx + 1 < tokens.len()
                        && tokens[current_idx + 1].token_type == TokenType::PlusEquals
                    {
                        let mod_idx = current_idx + 2;
                        if mod_idx < tokens.len() {
                            let mod_val = tokens[mod_idx].value.clone();
                            if let Some(sym) = symbol_table.symbols.get(&var_name) {
                                if !sym.changeable {
                                    println!(
                                        "\x1b[1;31mCompile Error:\x1b[0m Cannot modify immutable variable or constant '{}'.",
                                        var_name
                                    );
                                    std::process::exit(1);
                                }

                                let mod_data = if tokens[mod_idx].token_type
                                    == TokenType::NumberLiteral
                                {
                                    if sym.var_type == "i4M" {
                                        AholaData::I4M(
                                            mod_val.parse::<BigInt>().unwrap_or(BigInt::zero()),
                                        )
                                    } else if sym.var_type == "u4M" {
                                        AholaData::U4M(
                                            mod_val.parse::<BigUint>().unwrap_or(BigUint::zero()),
                                        )
                                    } else {
                                        AholaData::Int5M(
                                            mod_val.parse::<BigInt>().unwrap_or(BigInt::zero()),
                                        )
                                    }
                                } else {
                                    AholaData::String(mod_val)
                                };

                                bytecode.push(OpCode::PlusEqual(sym.slot, mod_data));
                                idx = mod_idx + 1;
                                continue;
                            }
                        }
                    }

                    current_idx += 1;
                    if current_idx < tokens.len()
                        && tokens[current_idx].token_type == TokenType::ChangeableModifier
                    {
                        if is_constant {
                            println!(
                                "\x1b[1;31mCompile Error:\x1b[0m Constants ('{}') cannot have the '*c' changeable modifier.",
                                var_name
                            );
                            std::process::exit(1);
                        }
                        is_changeable = true;
                        current_idx += 1;
                    }

                    let mut is_private = false;
                    let mut explicit_type = "deduced".to_string();
                    if current_idx < tokens.len()
                        && tokens[current_idx].token_type == TokenType::Colon
                    {
                        is_private = true;
                        if current_idx + 1 < tokens.len() {
                            explicit_type = tokens[current_idx + 1].value.clone();
                            current_idx += 2;
                        }
                    }

                    if current_idx < tokens.len()
                        && tokens[current_idx].token_type == TokenType::Equals
                    {
                        let value_idx = current_idx + 1;
                        if value_idx < tokens.len() {
                            let raw_val = tokens[value_idx].value.clone();

                            if tokens[value_idx].token_type == TokenType::Identifier {
                                println!(
                                    "\x1b[1;31mCompile Error:\x1b[0m Invalid assignment format in '{} = {}'. Right side must be a literal.",
                                    var_name, raw_val
                                );
                                std::process::exit(1);
                            }

                            let data = if tokens[value_idx].token_type == TokenType::NumberLiteral {
                                if explicit_type == "u4M" {
                                    AholaData::U4M(
                                        raw_val.parse::<BigUint>().unwrap_or(BigUint::zero()),
                                    )
                                } else if explicit_type == "int5M" {
                                    AholaData::Int5M(
                                        raw_val.parse::<BigInt>().unwrap_or(BigInt::zero()),
                                    )
                                } else {
                                    // Default to standard signed 4M width
                                    AholaData::I4M(
                                        raw_val.parse::<BigInt>().unwrap_or(BigInt::zero()),
                                    )
                                }
                            } else if tokens[value_idx].token_type == TokenType::LeftBracket {
                                let mut items = Vec::new();
                                let mut scan = value_idx + 1;
                                while scan < tokens.len()
                                    && tokens[scan].token_type != TokenType::RightBracket
                                {
                                    if tokens[scan].token_type == TokenType::StringLiteral {
                                        items.push(AholaData::String(tokens[scan].value.clone()));
                                    } else if tokens[scan].token_type == TokenType::NumberLiteral {
                                        items.push(AholaData::I4M(
                                            tokens[scan]
                                                .value
                                                .parse::<BigInt>()
                                                .unwrap_or(BigInt::zero()),
                                        ));
                                    }
                                    scan += 1;
                                }
                                current_idx = scan;
                                AholaData::Card(items)
                            } else {
                                AholaData::String(raw_val)
                            };

                            let final_type = if explicit_type == "deduced" {
                                match data {
                                    AholaData::I4M(_) => "i4M".to_string(),
                                    AholaData::U4M(_) => "u4M".to_string(),
                                    AholaData::Int5M(_) => "int5M".to_string(),
                                    AholaData::Card(_) => "card".to_string(),
                                    _ => "string".to_string(),
                                }
                            } else {
                                explicit_type
                            };

                            let slot = if let Some(sym) = symbol_table.symbols.get(&var_name) {
                                sym.slot
                            } else {
                                symbol_table.register(
                                    &var_name,
                                    final_type,
                                    is_changeable,
                                    is_private,
                                )
                            };

                            bytecode.push(OpCode::Store(slot, data));
                            idx = if tokens[value_idx].token_type == TokenType::LeftBracket {
                                current_idx + 1
                            } else {
                                value_idx + 1
                            };
                            continue;
                        }
                    }
                }
                idx += 1;
            }
            TokenType::Pub => {
                if idx + 3 < tokens.len()
                    && tokens[idx + 1].token_type == TokenType::Struct
                    && tokens[idx + 2].value == "@main"
                    && tokens[idx + 3].token_type == TokenType::LeftBrace
                {
                    let mut inner_tokens = Vec::new();
                    let mut scan = idx + 4;
                    let mut brace_count = 1;

                    while scan < tokens.len() && brace_count > 0 {
                        if tokens[scan].token_type == TokenType::LeftBrace {
                            brace_count += 1;
                        }
                        if tokens[scan].token_type == TokenType::RightBrace {
                            brace_count -= 1;
                        }
                        if brace_count > 0 {
                            inner_tokens.push(tokens[scan].clone());
                        }
                        scan += 1;
                    }
                    compile_tokens(&inner_tokens, symbol_table, bytecode);
                    idx = scan;
                } else {
                    idx += 1;
                }
            }
            TokenType::Stamp => {
                if idx + 1 < tokens.len() {
                    bytecode.push(OpCode::Stamp(tokens[idx + 1].value.clone()));
                    idx += 2;
                } else {
                    idx += 1;
                }
            }
            TokenType::Dump => {
                if idx + 1 < tokens.len() {
                    let target = &tokens[idx + 1].value;
                    if let Some(sym) = symbol_table.symbols.get(target) {
                        bytecode.push(OpCode::Dump(sym.slot));
                    }
                    idx += 2;
                } else {
                    idx += 1;
                }
            }
            TokenType::CallRust => {
                if idx + 2 < tokens.len() && tokens[idx + 1].token_type == TokenType::Identifier {
                    bytecode.push(OpCode::CallRustFunc {
                        func_name: tokens[idx + 1].value.clone(),
                        arg: tokens[idx + 2].value.clone(),
                    });
                    idx += 3;
                } else {
                    idx += 1;
                }
            }
            TokenType::FileHook => {
                bytecode.push(OpCode::ExternalFileHook(tokens[idx].value.clone()));
                idx += 1;
            }
            TokenType::Embed => {
                if idx + 3 < tokens.len()
                    && tokens[idx + 1].value == "rust"
                    && tokens[idx + 2].token_type == TokenType::LeftBrace
                {
                    let mut rust_code = String::new();
                    let mut scan = idx + 3;
                    while scan < tokens.len() && tokens[scan].token_type != TokenType::RightBrace {
                        rust_code.push_str(&tokens[scan].value);
                        rust_code.push(' ');
                        scan += 1;
                    }
                    bytecode.push(OpCode::InlineRust(rust_code));
                    idx = scan + 1;
                } else {
                    idx += 1;
                }
            }
            _ => idx += 1,
        }
    }
}

pub fn optimize_bytecode(bytecode: Vec<OpCode>) -> Vec<OpCode> {
    let mut optimized = Vec::new();
    let mut used_slots = HashSet::new();

    for instr in &bytecode {
        match instr {
            OpCode::Dump(slot) | OpCode::PlusEqual(slot, _) => {
                used_slots.insert(*slot);
            }
            _ => {}
        }
    }

    for instr in bytecode {
        match instr {
            OpCode::Store(slot, _) => {
                if used_slots.contains(&slot) || slot == 0 {
                    optimized.push(instr);
                }
            }
            _ => optimized.push(instr),
        }
    }
    optimized
}

pub struct VirtualMachine {
    pub memory: Vec<Option<AholaData>>,
}

impl VirtualMachine {
    pub fn new(slots: usize) -> Self {
        VirtualMachine {
            memory: vec![None; slots + 50],
        }
    }

    pub fn run(&mut self, bytecode: &[OpCode], symbol_table: &SymbolTable) {
        let mut pc = 0;
        while pc < bytecode.len() {
            match &bytecode[pc] {
                OpCode::Store(slot, data) => {
                    if *slot < self.memory.len() {
                        self.memory[*slot] = Some(data.clone());
                    }
                }
                OpCode::PlusEqual(slot, modifier) => {
                    if *slot < self.memory.len() {
                        if let Some(Some(existing)) = self.memory.get_mut(*slot) {
                            match (existing, modifier) {
                                (AholaData::I4M(old), AholaData::I4M(m)) => *old += m,
                                (AholaData::U4M(old), AholaData::U4M(m)) => *old += m,
                                (AholaData::Int5M(old), AholaData::Int5M(m)) => *old += m,
                                (AholaData::String(old), AholaData::String(m)) => old.push_str(m),
                                (AholaData::Card(old), m) => old.push(m.clone()),
                                _ => panic!(
                                    "VM Runtime Exception: Type mutation mismatch register slot!"
                                ),
                            }
                        }
                    }
                }
                OpCode::Stamp(message) => {
                    let mut output_str = message.clone();
                    for (name, symbol) in &symbol_table.symbols {
                        let pattern = format!("\\({})", name);
                        if let Some(Some(data)) = self.memory.get(symbol.slot) {
                            let format_val = match data {
                                AholaData::I4M(i) => i.to_string(),
                                AholaData::U4M(u) => u.to_string(),
                                AholaData::Int5M(u) => u.to_string(),
                                AholaData::Float(f) => f.to_string(),
                                AholaData::String(s) => s.clone(),
                                AholaData::Card(items) => format!("{:?}", items),
                                AholaData::DbRef(db) => format!(".db reference ({})", db),
                                AholaData::None => "None".to_string(),
                            };
                            output_str = output_str.replace(&pattern, &format_val);
                        }
                    }
                    println!("{}", output_str);
                }
                OpCode::Dump(slot) => {
                    if *slot < self.memory.len() {
                        if let Some(Some(data)) = self.memory.get(*slot) {
                            println!("{:?}", data);
                        }
                    }
                }
                OpCode::InlineRust(raw_code) => {
                    println!(
                        "[VM Embed-Rust Executing]: Object block target: {}",
                        raw_code.trim()
                    );
                }
                OpCode::CallRustFunc { func_name, arg } => {
                    println!(
                        "[Native Rust FFI Link]: Invoking {} with parameter '{}'",
                        func_name, arg
                    );
                }
                OpCode::ExternalFileHook(file_target) => {
                    println!(
                        "[VM Vector Map Link]: Dynamic binding established to native module -> {}",
                        file_target
                    );
                }
                OpCode::Jump(target) | OpCode::JumpIfFalse(target) => {
                    pc = *target;
                    continue;
                }
                OpCode::ReadFile(target_slot, filename_slot) => {
                    if let Some(Some(AholaData::String(filename))) = self.memory.get(*filename_slot)
                    {
                        if let Ok(contents) = fs::read_to_string(filename) {
                            self.memory[*target_slot] = Some(AholaData::String(contents));
                        }
                    }
                }
                OpCode::DbRangeRequest {
                    target_slot,
                    db_var_slot,
                    start,
                    end,
                } => {
                    if let Some(Some(AholaData::DbRef(db_name))) = self.memory.get(*db_var_slot) {
                        let db = sled::open(format!("{}.db", db_name)).unwrap();
                        let mut loaded_cards = Vec::new();

                        let mut loop_idx = start.clone();
                        while loop_idx <= *end {
                            let key_bytes = loop_idx.to_bytes_be();
                            if let Some(ivec) = db.get(&key_bytes).unwrap() {
                                let val_string =
                                    String::from_utf8(ivec.to_vec()).unwrap_or_default();
                                loaded_cards.push(AholaData::String(val_string));
                            }
                            loop_idx += BigUint::one();
                        }

                        match target_slot {
                            Some(slot) => {
                                // Save inside targeted memory space allocation slot
                                self.memory[*slot] = Some(AholaData::Card(loaded_cards));
                            }
                            None => {
                                // Standalone call execution output dump stream
                                println!("[Pure Request Stream Output]: {:?}", loaded_cards);
                            }
                        }
                    }
                }
                OpCode::Ban(slot, name) => {
                    self.memory[*slot] = None;
                    let _ = fs::remove_dir_all(format!("{}.db", name));
                    println!("Asset '{}' has been permanently banned and purged.", name);
                }
                OpCode::Call(_) | OpCode::Return => {}
            }
            pc += 1;
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

    let ahola_code = match fs::read_to_string(file_path) {
        Ok(code) => code,
        Err(_) => {
            println!(
                "\x1b[1;31mError:\x1b[0m No such file or directory: '{}'",
                file_path
            );
            std::process::exit(1);
        }
    };

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

    println!(
        "{}Compiling{} {}",
        bold_hex_dark_green, reset_color, file_path
    );

    let mut symbol_table = SymbolTable::new();
    let mut bytecode = Vec::new();

    compile_tokens(&tokens, &mut symbol_table, &mut bytecode);
    let optimized_bytecode = optimize_bytecode(bytecode);

    let duration = start_time.elapsed().as_millis();
    println!(
        "{}Compiled & Optimized{} {} in {}ms",
        bold_hex_dark_green, reset_color, file_path, duration
    );

    let mut vm = VirtualMachine::new(symbol_table.next_slot);
    vm.run(&optimized_bytecode, &symbol_table);
}
