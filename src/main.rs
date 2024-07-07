use std::env;
use std::fs;
use std::io::{self, Write};

#[derive(Debug)]
#[allow(non_camel_case_types)]
enum Token{
    LEFT_PAREN,
    RIGHT_PAREN,
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

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            if !file_contents.is_empty() {
                for x in file_contents.chars() {
                    match x {
                        '(' => {
                            println!("{:?} {x} null", Token::LEFT_PAREN)
                        }
                        ')' => {
                            println!("{:?} {x} null", Token::RIGHT_PAREN)
                        }
                        _ => {
                            panic!("unsupported char {x}")
                        }
                    }
                }
                println!("{:?}  null", Token::EOF)
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
