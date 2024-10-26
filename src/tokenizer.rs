use std::{fs};
use std::str::Chars;
use std::fmt::{Display, Formatter};
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[allow(non_camel_case_types)]
#[allow(unused)]
pub enum Token {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    STAR,
    DOT,
    COMMA,
    MINUS,
    PLUS,
    SEMICOLON,
    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    SLASH,
    COMMENT,
    STRING,
    NUMBER,
    IDENTIFIER,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    EOF,
}

impl Token {
    pub fn is_literal(&self) -> bool {
        match self {
            Token::STRING => { true }
            Token::NUMBER => { true }
            Token::FALSE => { true }
            Token::NIL => { true }
            Token::TRUE => { true }
            _ => { false }
        }
    }

    pub fn is_binary_operator(&self) -> bool {
        match self {
            Token::MINUS => { true }
            Token::PLUS => { true }
            Token::STAR => { true }
            Token::SLASH => { true }
            Token::GREATER => { true }
            Token::GREATER_EQUAL => { true }
            Token::LESS => { true }
            Token::LESS_EQUAL => { true }
            Token::BANG_EQUAL => { true }
            Token::EQUAL_EQUAL => { true }
            Token::EQUAL => { true }
            Token::OR => { true }
            Token::AND => { true }
            _ => { false }
        }
    }
}

pub struct CharIterator {
    position: usize,
    limit: usize,
    chars: Vec<char>,
    pub line: usize,
    pub line_position: usize,
}

impl CharIterator {
    pub fn from(chars: Chars) -> CharIterator {
        let chars: Vec<char> = chars.collect();
        CharIterator {
            position: 0,
            limit: chars.len(),
            chars,
            line: 1,
            line_position: 1,
        }
    }
    pub fn peek(&self) -> Option<char> {
        if self.position >= self.limit {
            None
        } else {
            Some(self.chars[self.position])
        }
    }
    pub fn advance(&mut self) {
        self.position += 1;
        if let Some(c) = self.peek() {
            if c == '\n' {
                self.line += 1;
                self.line_position = 1;
            }
        }
    }
}

#[allow(unused)]
#[derive(Clone)]
pub struct Lexeme {
    pub token: Token,
    pub src: Vec<char>,
    pub value: Vec<char>,
    pub line: usize,
    pub line_position: usize,
}

impl Display for Lexeme {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let src_str: String = self.src.iter().collect();
        let value_str: String = self.value.iter().collect();
        write!(f, "{:?} {} {}", self.token, src_str, value_str)
    }
}

pub fn tokenize_file(filename: &String) -> (Vec<Lexeme>, Result<(), ()>) {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {filename}");
        String::new()
    });
    tokenize(file_contents.chars())
}

pub fn tokenize(data: Chars) -> (Vec<Lexeme>, Result<(), ()>) {
    let tokens = get_tokens_map();
    let allowed_chars = get_allowed_chars_set();

    let mut chars = vec![];
    let mut has_lexical_errors = false;
    let mut lexemes: Vec<Lexeme> = vec![];

    let mut iterator = CharIterator::from(data);
    if let Some(c) = iterator.peek() {
        if c == '\n' {
            iterator.line += 1;
            iterator.line_position = 1;
        }
    }

    while let Some(c) = iterator.peek() {
        if c == '\n' || c == ' ' || c == '\t' {
            iterator.advance();
            continue;
        }
        if c.is_ascii_digit() {
            if flush_token(&tokens, &mut chars, &mut iterator, &mut lexemes) {
                continue;
            }

            read_number(&mut iterator, &mut chars, &mut lexemes);
            continue;
        } else if c == '"' {
            if flush_token(&tokens, &mut chars, &mut iterator, &mut lexemes) {
                continue;
            }

            if let Err(_) = read_string(c, &mut iterator, &mut chars, &mut lexemes) {
                has_lexical_errors = true;
            }
            continue;
        } else if !allowed_chars.contains(&c) {
            if flush_token(&tokens, &mut chars, &mut iterator, &mut lexemes) {
                continue;
            }
            if c.is_ascii_alphabetic() || c == '_' {
                read_identifier(&mut iterator, &mut chars, &tokens, &mut lexemes);
                continue;
            }

            let line_number = iterator.line;
            eprintln!("[line {line_number}] Error: Unexpected character: {c}");
            has_lexical_errors = true;
        } else {
            chars.push(c);
            match tokens.get(chars.as_slice()) {
                None => {
                    chars.pop();
                    if flush_token(&tokens, &mut chars, &mut iterator, &mut lexemes) {
                        continue;
                    }
                    chars.push(c);
                }
                Some(_) => {}
            }
        }
        iterator.advance();
    }

    match tokens.get(chars.as_slice()) {
        None => {}
        Some(token) => {
            if token != &Token::COMMENT {
                print_token(chars.as_slice(), token, &mut lexemes, &iterator);
            }
            chars.clear();
        }
    }

    if !chars.is_empty() {
        panic!("cannot find token for {:?}", chars)
    }
    // lexemes.push(Lexeme {
    //     token: Token::EOF,
    //     src: Vec::with_capacity(0),
    //     value: "null".chars().collect(),
    //     line: iterator.line,
    //     line_position: iterator.line_position,
    // });
    return (lexemes, match has_lexical_errors {
        true => { Err(()) }
        false => { Ok(()) }
    });
}

fn flush_token(tokens: &HashMap<Box<[char]>, Token>, mut chars: &mut Vec<char>, mut iterator: &mut CharIterator, lexemes: &mut Vec<Lexeme>) -> bool {
    if chars.len() > 0 {
        match tokens.get(chars.as_slice()) {
            None => {}
            Some(token) => {
                if token == &Token::COMMENT {
                    let comment = read_comment(&mut iterator, &mut chars);
                    eprintln!("comment: {comment}");
                    return true;
                } else {
                    print_token(chars.as_slice(), token, lexemes, &iterator);
                }
                chars.clear();
            }
        }
    }
    false
}

fn read_comment(iterator: &mut CharIterator, chars: &mut Vec<char>) -> String {
    loop {
        match iterator.peek() {
            None => {
                break
            }
            Some(c) => {
                iterator.advance();
                if c == '\n' {
                    break;
                } else {
                    chars.push(c);
                }
            }
        }
    }
    let comment = String::from_iter(chars.iter());
    chars.clear();
    comment
}

fn read_identifier(iterator: &mut CharIterator, chars: &mut Vec<char>, tokens: &HashMap<Box<[char]>, Token>, lexemes: &mut Vec<Lexeme>) {
    loop {
        match iterator.peek() {
            None => {
                if let Some(token) = tokens.get(chars.as_slice()) {
                    print_token(chars.as_slice(), token, lexemes, &iterator);
                } else {
                    print_token(chars.as_slice(), &Token::IDENTIFIER, lexemes, &iterator);
                }
                chars.clear();
                break;
            }
            Some(c) => {
                if c == '_' || c.is_ascii_alphanumeric() {
                    chars.push(c);
                    iterator.advance();
                } else {
                    if let Some(token) = tokens.get(chars.as_slice()) {
                        print_token(chars.as_slice(), token, lexemes, &iterator);
                    } else {
                        print_token(chars.as_slice(), &Token::IDENTIFIER, lexemes, &iterator);
                    }
                    chars.clear();
                    break;
                }
            }
        }
    }
}

fn read_string(c: char, iterator: &mut CharIterator, chars: &mut Vec<char>, lexemes: &mut Vec<Lexeme>) -> Result<(), ()> {
    chars.push(c);
    iterator.advance();
    let quote_char = c;

    loop {
        match iterator.peek() {
            None => {
                let line_number = iterator.line;
                eprintln!("[line {line_number}] Error: Unterminated string.");
                chars.clear();
                return Err(());
            }
            Some(c) => {
                chars.push(c);
                iterator.advance();
                if c == quote_char && (chars.len() == 0 || chars[chars.len() - 1] != '\\') {
                    print_token(chars.as_slice(), &Token::STRING, lexemes, &iterator);
                    chars.clear();
                    return Ok(());
                }
            }
        }
    }
}

fn read_number(iterator: &mut CharIterator, chars: &mut Vec<char>, lexemes: &mut Vec<Lexeme>) {
    let mut no_dots = true;
    loop {
        match iterator.peek() {
            None => {
                if chars[chars.len() - 1] == '.' {
                    print_token(&chars.as_slice()[..chars.len() - 1], &Token::NUMBER, lexemes, &iterator);

                    chars.clear();
                    chars.push('.');
                    print_token(chars.as_slice(), &Token::DOT, lexemes, &iterator);
                } else {
                    print_token(chars.as_slice(), &Token::NUMBER, lexemes, &iterator);
                }
                chars.clear();
                iterator.advance();
                break;
            }
            Some(c) => {
                if c.is_ascii_digit() {
                    chars.push(c)
                } else if c == '.' {
                    if no_dots {
                        chars.push(c);
                        no_dots = false;
                    } else {
                        // panic!("cannot parse number")
                        print_token(chars.as_slice(), &Token::NUMBER, lexemes, &iterator);
                        chars.clear();

                        // chars.push('.');
                        // print_token(chars.as_slice(), &Token::DOT);
                        // chars.clear();
                        break;
                    }
                } else {
                    print_token(chars.as_slice(), &Token::NUMBER, lexemes, &iterator);
                    chars.clear();
                    break;
                }
                iterator.advance();
            }
        }
    }
}

pub fn get_tokens_map() -> HashMap<Box<[char]>, Token> {
    let mut tokens: HashMap<Box<[char]>, Token> = HashMap::new();
    tokens.insert(str_to_slice("("), Token::LEFT_PAREN);
    tokens.insert(str_to_slice(")"), Token::RIGHT_PAREN);
    tokens.insert(str_to_slice("{"), Token::LEFT_BRACE);
    tokens.insert(str_to_slice("}"), Token::RIGHT_BRACE);
    tokens.insert(str_to_slice("*"), Token::STAR);
    tokens.insert(str_to_slice("."), Token::DOT);
    tokens.insert(str_to_slice(","), Token::COMMA);
    tokens.insert(str_to_slice("-"), Token::MINUS);
    tokens.insert(str_to_slice("+"), Token::PLUS);
    tokens.insert(str_to_slice(";"), Token::SEMICOLON);
    tokens.insert(str_to_slice("="), Token::EQUAL);
    tokens.insert(str_to_slice("=="), Token::EQUAL_EQUAL);
    tokens.insert(str_to_slice("!"), Token::BANG);
    tokens.insert(str_to_slice("!="), Token::BANG_EQUAL);
    tokens.insert(str_to_slice("<"), Token::LESS);
    tokens.insert(str_to_slice("<="), Token::LESS_EQUAL);
    tokens.insert(str_to_slice(">"), Token::GREATER);
    tokens.insert(str_to_slice(">="), Token::GREATER_EQUAL);
    tokens.insert(str_to_slice("/"), Token::SLASH);
    tokens.insert(str_to_slice("//"), Token::COMMENT);
    tokens.insert(str_to_slice("and"), Token::AND);
    tokens.insert(str_to_slice("class"), Token::CLASS);
    tokens.insert(str_to_slice("else"), Token::ELSE);
    tokens.insert(str_to_slice("false"), Token::FALSE);
    tokens.insert(str_to_slice("for"), Token::FOR);
    tokens.insert(str_to_slice("fun"), Token::FUN);
    tokens.insert(str_to_slice("if"), Token::IF);
    tokens.insert(str_to_slice("nil"), Token::NIL);
    tokens.insert(str_to_slice("or"), Token::OR);
    tokens.insert(str_to_slice("print"), Token::PRINT);
    tokens.insert(str_to_slice("return"), Token::RETURN);
    tokens.insert(str_to_slice("super"), Token::SUPER);
    tokens.insert(str_to_slice("this"), Token::THIS);
    tokens.insert(str_to_slice("true"), Token::TRUE);
    tokens.insert(str_to_slice("var"), Token::VAR);
    tokens.insert(str_to_slice("while"), Token::WHILE);
    tokens
}

fn get_allowed_chars_set() -> HashSet<char> {
    let mut chars: HashSet<char> = HashSet::new();
    chars.insert('(');
    chars.insert(')');
    chars.insert('{');
    chars.insert('}');
    chars.insert('*');
    chars.insert('.');
    chars.insert(',');
    chars.insert('-');
    chars.insert('+');
    chars.insert(';');
    chars.insert('=');
    chars.insert('!');
    chars.insert('<');
    chars.insert('>');
    chars.insert('/');
    chars
}

fn print_token(chars: &[char], token: &Token, lexemes: &mut Vec<Lexeme>, iterator: &CharIterator) {
    if token == &Token::STRING {
        lexemes.push(Lexeme {
            token: *token,
            src: chars.to_vec(),
            value: Vec::from(&chars[1..chars.len() - 1]),
            line: iterator.line,
            line_position: iterator.line_position,
        });
    } else if token == &Token::NUMBER {
        let s = String::from_iter(chars.iter());
        let value: f64 = s.parse().expect("failed to parse number");

        let number_str = value.to_string();
        let mut x: Vec<char> = number_str.chars().collect();
        if !x.contains(&'.') {
            x.push('.');
            x.push('0');
        }

        lexemes.push(Lexeme {
            token: *token,
            src: chars.to_vec(),
            value: x,
            line: iterator.line,
            line_position: iterator.line_position,
        });
    } else {
        lexemes.push(Lexeme {
            token: *token,
            src: chars.to_vec(),
            value: "null".chars().collect(),
            line: iterator.line,
            line_position: iterator.line_position,
        });
    }
}

fn str_to_slice(s: &str) -> Box<[char]> {
    s.chars().collect::<Vec<char>>().into_boxed_slice()
}