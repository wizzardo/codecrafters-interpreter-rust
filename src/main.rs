use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::{self, Write};

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
    EOF,
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
            let mut line_number = 1;
            let mut _position = 0;

            if !file_contents.is_empty() {
                for c in file_contents.chars() {
                    _position += 1;
                    if c == '\n' {
                        line_number += 1;
                        _position = 0;
                    }
                    if !allowed_chars.contains(&c) {
                        writeln!(io::stderr(), "[line {line_number}] Error: Unexpected character: {c}").unwrap();
                        has_lexical_errors = true;

                        match tokens.get(chars.as_slice()) {
                            None => {}
                            Some(token) => {
                                print_token(chars.as_slice(), token);
                                chars.clear();
                            }
                        }
                        continue;
                    }


                    chars.push(c);
                    // match tokens.get(chars.as_slice()) {
                    //     None => {}
                    //     Some(token) => {
                    //         chars.clear();
                    //         print_token(c, token);
                    //     }
                    // }
                    match tokens.get(chars.as_slice()) {
                        None => {
                            let prev = &chars.as_slice()[..chars.len() - 1];
                            match tokens.get(prev) {
                                None => {}
                                Some(token) => {
                                    print_token(prev, token);
                                    chars.clear();
                                    chars.push(c);
                                }
                            };
                        }
                        Some(_) => {}
                    }
                }
            }

            match tokens.get(chars.as_slice()) {
                None => {}
                Some(token) => {
                    print_token(chars.as_slice(), token);
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
    chars
}

fn print_token(chars: &[char], token: &Token) {
    // println!("{:?} {x} null", token)
    print!("{:?} ", token);
    for c in chars {
        print!("{c}");
    }
    println!(" null");
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