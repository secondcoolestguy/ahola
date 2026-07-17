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
    Pub,
    Struct,
    ChangeableModifier,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AholaData {
    Integer(i64),
    Float(f64),
    String(String),
    Card(Vec<AholaData>),
}

#[derive(Debug, Clone)]
pub enum OpCode {
    Store(usize, AholaData),
    PlusEqual(usize, AholaData),
    Stamp(String),
    Dump(usize),
}

pub struct Symbol {
    pub slot: usize,
    pub var_type: String,
    pub changeable: bool,
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
    pub fn register(&mut self, name: &str, var_type: String, changeable: bool) -> usize {
        let slot = self.next_slot;
        self.symbols.insert(
            name.to_string(),
            Symbol {
                slot,
                var_type,
                changeable,
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
                return "0".to_string();
            }
            parts[0].ceil().to_string()
        }
        "round_down" => {
            if parts.is_empty() {
                return "0".to_string();
            }
            parts[0].floor().to_string()
        }
        "abs" => {
            if parts.is_empty() {
                return "0".to_string();
            }
            parts[0].abs().to_string()
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
                    "let" => tokens.push(Token {
                        token_type: TokenType::Let,
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
                    _ => {
                        if word.is_empty() {
                            continue;
                        }
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

pub fn compile_tokens(
    tokens: &[Token],
    symbol_table: &mut SymbolTable,
    bytecode: &mut Vec<OpCode>,
) {
    let mut idx = 0;
    while idx < tokens.len() {
        match tokens[idx].token_type {
            TokenType::Let | TokenType::Disguise | TokenType::Identifier => {
                let mut current_idx = idx;
                let mut is_changeable = false;

                if tokens[current_idx].token_type == TokenType::Let
                    || tokens[current_idx].token_type == TokenType::Disguise
                {
                    current_idx += 1;
                }

                if current_idx < tokens.len()
                    && tokens[current_idx].token_type == TokenType::Identifier
                {
                    let var_name = tokens[current_idx].value.clone();

                    if current_idx + 1 < tokens.len()
                        && tokens[current_idx + 1].token_type == TokenType::PlusEquals
                    {
                        let mod_idx = current_idx + 2;
                        if mod_idx < tokens.len() {
                            let mod_val = tokens[mod_idx].value.clone();
                            if let Some(sym) = symbol_table.symbols.get(&var_name) {
                                if !sym.changeable {
                                    println!(
                                        "\x1b[1;31mCompile Error:\x1b[0m Cannot modify immutable variable '{}'.",
                                        var_name
                                    );
                                    std::process::exit(1);
                                }

                                let mod_data =
                                    if tokens[mod_idx].token_type == TokenType::NumberLiteral {
                                        if mod_val.contains('.') {
                                            AholaData::Float(mod_val.parse().unwrap_or(0.0))
                                        } else {
                                            AholaData::Integer(mod_val.parse().unwrap_or(0))
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
                        is_changeable = true;
                        current_idx += 1;
                    }

                    let mut explicit_type = "deduced".to_string();
                    if current_idx < tokens.len()
                        && tokens[current_idx].token_type == TokenType::Colon
                    {
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

                            let data = if tokens[value_idx].token_type == TokenType::NumberLiteral {
                                if raw_val.contains('.') {
                                    AholaData::Float(raw_val.parse().unwrap_or(0.0))
                                } else {
                                    AholaData::Integer(raw_val.parse().unwrap_or(0))
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
                                        items.push(AholaData::Integer(
                                            tokens[scan].value.parse().unwrap_or(0),
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
                                    AholaData::Integer(_) | AholaData::Float(_) => {
                                        "int/float".to_string()
                                    }
                                    AholaData::Card(_) => "card".to_string(),
                                    _ => "string".to_string(),
                                }
                            } else {
                                explicit_type
                            };

                            let slot = if let Some(sym) = symbol_table.symbols.get(&var_name) {
                                sym.slot
                            } else {
                                symbol_table.register(&var_name, final_type, is_changeable)
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
                    let message = tokens[idx + 1].value.clone();
                    bytecode.push(OpCode::Stamp(message));
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
            _ => idx += 1,
        }
    }
}

pub struct VirtualMachine {
    pub memory: Vec<Option<AholaData>>,
}

impl VirtualMachine {
    pub fn new(slots: usize) -> Self {
        VirtualMachine {
            memory: vec![None; slots],
        }
    }

    pub fn run(&mut self, bytecode: &[OpCode], symbol_table: &SymbolTable) {
        for instruction in bytecode {
            match instruction {
                OpCode::Store(slot, data) => {
                    if *slot < self.memory.len() {
                        self.memory[*slot] = Some(data.clone());
                    }
                }
                OpCode::PlusEqual(slot, modifier) => {
                    if *slot < self.memory.len() {
                        if let Some(Some(existing)) = self.memory.get_mut(*slot) {
                            match (existing, modifier) {
                                (AholaData::Integer(old), AholaData::Integer(m)) => *old += m,
                                (AholaData::Float(old), AholaData::Float(m)) => *old += m,
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
                                AholaData::Integer(i) => i.to_string(),
                                AholaData::Float(f) => f.to_string(),
                                AholaData::String(s) => s.clone(),
                                AholaData::Card(items) => format!("{:?}", items),
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
            }
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

    let duration = start_time.elapsed().as_millis();
    println!(
        "{}Compiled{} {} in {}ms",
        bold_hex_dark_green, reset_color, file_path, duration
    );

    let mut vm = VirtualMachine::new(symbol_table.next_slot);
    vm.run(&bytecode, &symbol_table);
}
