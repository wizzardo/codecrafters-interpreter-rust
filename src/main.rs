use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::{self, Write};
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
enum Token {
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
    EOF,
}

struct CharIterator {
    position: usize,
    limit: usize,
    chars: Vec<char>,
    pub line: usize,
    pub line_position: usize,
}

impl CharIterator {
    fn from(chars: Chars) -> CharIterator {
        let chars: Vec<char> = chars.collect();
        CharIterator {
            position: 0,
            limit: chars.len(),
            chars,
            line: 1,
            line_position: 1,
        }
    }
    fn peek(&self) -> Option<char> {
        if self.position >= self.limit {
            None
        } else {
            Some(self.chars[self.position])
        }
    }
    fn advance(&mut self) {
        self.position += 1;
        if let Some(c) = self.peek() {
            if c == '\n' {
                self.line += 1;
                self.line_position = 1;
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    let tokens = get_tokens_map();
    let allowed_chars = get_allowed_chars_set();
    let mut has_lexical_errors = false;

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut chars = vec![];

            if !file_contents.is_empty() {
                let mut iterator = CharIterator::from(file_contents.chars());
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
                        if flush_token(&tokens, &mut chars, &mut iterator) {
                            continue;
                        }

                        read_number(&mut iterator, &mut chars);
                        continue;
                    } else if c == '"' {
                        if flush_token(&tokens, &mut chars, &mut iterator) {
                            continue;
                        }

                        if let Err(_) = read_string(c, &mut iterator, &mut chars) {
                            has_lexical_errors = true;
                        }
                        continue;
                    } else if !allowed_chars.contains(&c) {
                        if flush_token(&tokens, &mut chars, &mut iterator) {
                            continue;
                        }
                        if c.is_ascii_alphabetic() || c == '_' {
                            read_identifier(&mut iterator, &mut chars);
                            continue;
                        }

                        let line_number = iterator.line;
                        writeln!(io::stderr(), "[line {line_number}] Error: Unexpected character: {c}").unwrap();
                        has_lexical_errors = true;
                    } else {
                        chars.push(c);
                        match tokens.get(chars.as_slice()) {
                            None => {
                                chars.pop();
                                if flush_token(&tokens, &mut chars, &mut iterator) {
                                    continue;
                                }
                                chars.push(c);
                            }
                            Some(_) => {}
                        }
                    }
                    iterator.advance();
                }
            }

            match tokens.get(chars.as_slice()) {
                None => {}
                Some(token) => {
                    if token != &Token::COMMENT {
                        print_token(chars.as_slice(), token);
                    }
                    chars.clear();
                }
            }

            if !chars.is_empty() {
                panic!("cannot find token for {:?}", chars)
            }
            println!("{:?}  null", Token::EOF)
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }

    if has_lexical_errors {
        std::process::exit(65);
    }
}

fn read_identifier(iterator: &mut CharIterator, chars: &mut Vec<char>) {
    loop {
        match iterator.peek() {
            None => {
                print_token(chars.as_slice(), &Token::IDENTIFIER);
                chars.clear();
                break;
            }
            Some(c) => {
                if c == '_' || c.is_ascii_alphanumeric() {
                    chars.push(c);
                    iterator.advance();
                } else {
                    print_token(chars.as_slice(), &Token::IDENTIFIER);
                    chars.clear();
                    break;
                }
            }
        }
    }
}

fn flush_token(tokens: &HashMap<Box<[char]>, Token>, mut chars: &mut Vec<char>, mut iterator: &mut CharIterator) -> bool {
    if chars.len() > 0 {
        match tokens.get(chars.as_slice()) {
            None => {}
            Some(token) => {
                if token == &Token::COMMENT {
                    skip_until_next_line(&mut iterator, &mut chars);
                    return true;
                } else {
                    print_token(chars.as_slice(), token);
                }
                chars.clear();
            }
        }
    }
    false
}

fn skip_until_next_line(iterator: &mut CharIterator, chars: &mut Vec<char>) {
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
    writeln!(io::stderr(), "comment: {comment}").unwrap();
}

fn read_string(c: char, iterator: &mut CharIterator, chars: &mut Vec<char>) -> Result<(), ()> {
    chars.push(c);
    iterator.advance();
    let quote_char = c;

    loop {
        match iterator.peek() {
            None => {
                let line_number = iterator.line;
                writeln!(io::stderr(), "[line {line_number}] Error: Unterminated string.").unwrap();
                chars.clear();
                return Err(());
            }
            Some(c) => {
                chars.push(c);
                iterator.advance();
                if c == quote_char && (chars.len() == 0 || chars[chars.len() - 1] != '\\') {
                    print_token(chars.as_slice(), &Token::STRING);
                    chars.clear();
                    return Ok(());
                }
            }
        }
    }
}

fn read_number(iterator: &mut CharIterator, chars: &mut Vec<char>) {
    let mut no_dots = true;
    loop {
        match iterator.peek() {
            None => {
                if chars[chars.len() - 1] == '.' {
                    print_token(&chars.as_slice()[..chars.len() - 1], &Token::NUMBER);

                    chars.clear();
                    chars.push('.');
                    print_token(chars.as_slice(), &Token::DOT);
                } else {
                    print_token(chars.as_slice(), &Token::NUMBER);
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
                        print_token(chars.as_slice(), &Token::NUMBER);
                        chars.clear();

                        // chars.push('.');
                        // print_token(chars.as_slice(), &Token::DOT);
                        // chars.clear();
                        break;
                    }
                } else {
                    print_token(chars.as_slice(), &Token::NUMBER);
                    chars.clear();
                    break;
                }
                iterator.advance();
            }
        }
    }
}

fn get_tokens_map() -> HashMap<Box<[char]>, Token> {
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

fn print_token(chars: &[char], token: &Token) {
    // println!("{:?} {x} null", token)
    print!("{:?} ", token);
    for c in chars {
        print!("{c}");
    }
    if token == &Token::STRING {
        print!(" ");
        for c in &chars[1..chars.len() - 1] {
            print!("{c}");
        }
        println!("");
    } else if token == &Token::NUMBER {
        print!(" ");
        let s = String::from_iter(chars.iter());
        writeln!(io::stderr(), "parsing number: {s}").unwrap();
        let value: f64 = s.parse().expect("failed to parse number");
        // writeln!(io::stderr(), "parsing number value: {value}").unwrap();

        let number_str = value.to_string();
        print!("{number_str}");
        if number_str.contains('.') {
            println!("")
        } else {
            println!(".0")
        }
    } else {
        println!(" null");
    }
}

fn str_to_slice(s: &str) -> Box<[char]> {
    s.chars().collect::<Vec<char>>().into_boxed_slice()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let tokens = get_tokens_map();
        let mut v = vec![];
        v.push('(');
        let option = tokens.get(v.as_slice());
        let token = option.expect("should contain a token");
        assert_eq!(&Token::LEFT_PAREN, token);
    }
}