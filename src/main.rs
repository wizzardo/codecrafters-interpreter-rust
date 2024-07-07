use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Write};

#[derive(Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
enum Token{
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
    EOF
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

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut chars = vec![];

            if !file_contents.is_empty() {
                for x in file_contents.chars() {
                    chars.push(x);
                    match tokens.get(chars.as_slice()) {
                        None => {}
                        Some(token) => {
                            chars.clear();
                            print_token(x, token);
                        }
                    }
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
    tokens
}

fn print_token(x: char, token: &Token) {
    println!("{:?} {x} null", token)
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