use std::collections::{HashMap, HashSet};
use std::env;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::io::{self, Write};
use std::ops::Not;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[allow(non_camel_case_types)]
#[allow(unused)]
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
    fn is_literal(&self) -> bool {
        match self {
            Token::STRING => { true }
            Token::NUMBER => { true }
            Token::FALSE => { true }
            Token::NIL => { true }
            Token::TRUE => { true }
            _ => { false }
        }
    }

    fn is_binary_operand(&self) -> bool {
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
            _ => { false }
        }
    }
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

struct LexemeIterator {
    position: usize,
    limit: usize,
    lexemes: Vec<Lexeme>,
}

impl LexemeIterator {
    fn from(lexemes: Vec<Lexeme>) -> LexemeIterator {
        LexemeIterator {
            position: 0,
            limit: lexemes.len(),
            lexemes,
        }
    }
    fn peek(&self) -> Option<&Lexeme> {
        if self.position >= self.limit {
            None
        } else {
            Some(&self.lexemes[self.position])
        }
    }
    fn advance(&mut self) {
        self.position += 1;
    }
}

#[allow(unused)]
#[derive(Clone)]
struct Lexeme {
    token: Token,
    src: Vec<char>,
    value: Vec<char>,
    line: usize,
    line_position: usize,
}

impl Display for Lexeme {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let src_str: String = self.src.iter().collect();
        let value_str: String = self.value.iter().collect();
        write!(f, "{:?} {} {}", self.token, src_str, value_str)
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


    match command.as_str() {
        "tokenize" => {
            let (lexemes, result) = tokenize_file(filename);

            for l in lexemes {
                println!("{}", l)
            }
            println!("EOF  null");

            if result.is_err() {
                std::process::exit(65);
            }
        }
        "parse" => {
            let (lexemes, result) = tokenize_file(filename);
            if result.is_err() {
                std::process::exit(65);
            }
            let expression = parse_lexemes(lexemes);
            println!("{}", expression.to_string())
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}

trait Expression {
    fn to_string(&self) -> String;
}

struct LiteralExpression {
    lexeme: Lexeme,
    literal: Primitive,
}

#[allow(unused)]
struct GroupExpression {
    start: Lexeme,
    end: Lexeme,
    expression: Box<dyn Expression>,
}

#[allow(unused)]
struct UnaryNotExpression {
    lexeme: Lexeme,
    expression: Box<dyn Expression>,
}

#[allow(unused)]
struct UnaryMinusExpression {
    lexeme: Lexeme,
    expression: Box<dyn Expression>,
}

#[allow(unused)]
struct BinaryExpression {
    lexeme: Lexeme,
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl Expression for LiteralExpression {
    fn to_string(&self) -> String {
        // self.lexeme.value.iter().collect()
        match &self.literal {
            Primitive::Number(_it) => {
                self.lexeme.value.iter().collect()
            }
            Primitive::String(it) => { it.clone() }
            Primitive::Boolean(it) => { it.to_string() }
            Primitive::Nil => { "nil".to_string() }
        }
    }
}

impl Expression for GroupExpression {
    fn to_string(&self) -> String {
        format!("(group {})", self.expression.to_string())
    }
}

impl Expression for UnaryNotExpression {
    fn to_string(&self) -> String {
        format!("(! {})", self.expression.to_string())
    }
}

impl Expression for UnaryMinusExpression {
    fn to_string(&self) -> String {
        format!("(- {})", self.expression.to_string())
    }
}

impl Expression for BinaryExpression {
    fn to_string(&self) -> String {
        let action = match self.lexeme.token {
            Token::STAR => { "*" }
            Token::MINUS => { "-" }
            Token::PLUS => { "+" }
            Token::SLASH => { "/" }
            Token::GREATER => { ">" }
            Token::GREATER_EQUAL => { ">=" }
            Token::LESS => { "<" }
            Token::LESS_EQUAL => { "<=" }
            Token::EQUAL_EQUAL => { "==" }
            Token::BANG_EQUAL => { "!=" }
            t => { panic!("{:?} is not an action for binary expression", t) }
        };
        format!("({} {} {})", action, self.left.to_string(), self.right.to_string())
    }
}

enum Primitive {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

fn parse_lexemes(lexemes: Vec<Lexeme>) -> Box<dyn Expression> {
    let mut iterator = LexemeIterator::from(lexemes);
    return parse(&mut iterator);
}

fn parse(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let mut operands = vec![];
    let mut operations = vec![];
    while let Some(lexeme) = iterator.peek() {
        let expression = if let true = lexeme.token.is_literal() {
            let expression = to_literal_expression(lexeme);
            iterator.advance();
            expression
        } else if lexeme.token == Token::LEFT_PAREN {
            parse_group(iterator)
        } else if lexeme.token == Token::BANG {
            parse_unary_not(iterator)
        } else if lexeme.token == Token::MINUS {
            parse_unary_minus(iterator)
        } else {
            writeln!(io::stderr(), "unexpected token {:?}", lexeme.token).unwrap();
            std::process::exit(65);
        };

        operands.push(expression);
        match iterator.peek() {
            None => {
                // return expression
                break
            }
            Some(lexeme) => {
                if lexeme.token.is_binary_operand() {
                    let lexeme = lexeme.clone();
                    operations.push(lexeme);
                    iterator.advance();
                    // let right = parse(iterator);
                    // break
                    // return Box::new(BinaryExpression { lexeme, left, right });
                } else {
                    // return expression;
                    break
                }
            }
        }
    }
    if operands.len() == 1 {
        return operands.into_iter().next().unwrap()
    } else if operands.len() == 0 {
        panic!("no expression found")
    } else {
        operands.reverse();
        operations.reverse();

        // for x in &operands {
        //     println!("{}", x.to_string());
        // }
        // for x in &operations {
        //     println!("{:?}", x.token);
        // }

        let lexeme = operations.remove(operations.len() - 1);
        let left = operands.remove(operands.len() - 1);
        let right = operands.remove(operands.len() - 1);
        let mut exp = BinaryExpression { lexeme, left, right };

        while operands.is_empty().not() {
            let right = operands.remove(operands.len() - 1);
            let lexeme = operations.remove(operations.len() - 1);
            exp = BinaryExpression { lexeme, left: Box::new(exp), right };
        }

        Box::new(exp)
    }
}

fn parse_one(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    while let Some(lexeme) = iterator.peek() {
        let expression = if let true = lexeme.token.is_literal() {
            let expression = to_literal_expression(lexeme);
            iterator.advance();
            expression
        } else if lexeme.token == Token::LEFT_PAREN {
            parse_group(iterator)
        } else if lexeme.token == Token::BANG {
            parse_unary_not(iterator)
        } else if lexeme.token == Token::MINUS {
            parse_unary_minus(iterator)
        } else {
            panic!("not implemented yet: {:?}", lexeme.token)
        };

        return expression;
    }
    panic!("no expression found")
}

fn parse_group(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let start = iterator.peek().unwrap().clone();
    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            writeln!(io::stderr(), "empty group expression").unwrap();
            std::process::exit(65);
        }
    }

    let expression = parse(iterator);
    let end: Lexeme;
    match iterator.peek() {
        None => {
            writeln!(io::stderr(), "unclosed group expression").unwrap();
            std::process::exit(65);
        }
        Some(lexeme) => {
            if lexeme.token != Token::RIGHT_PAREN {
                writeln!(io::stderr(), "{:?} != Token::RIGHT_PAREN", lexeme.token).unwrap();
                std::process::exit(65);
            }

            end = lexeme.clone();
        }
    };
    iterator.advance();
    Box::new(GroupExpression { start, end, expression })
}

fn parse_unary_not(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    Box::new(UnaryNotExpression { lexeme, expression: parse(iterator) })
}

fn parse_unary_minus(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    Box::new(UnaryMinusExpression { lexeme, expression: parse_one(iterator) })
}

fn to_literal_expression(lexeme: &Lexeme) -> Box<LiteralExpression> {
    let literal = match lexeme.token {
        Token::TRUE => { Primitive::Boolean(true) }
        Token::FALSE => { Primitive::Boolean(false) }
        Token::STRING => {
            let s: String = lexeme.value.iter().collect();
            Primitive::String(s)
        }
        Token::NUMBER => {
            let s: String = lexeme.value.iter().collect();
            Primitive::Number(s.parse().expect("failed to parse number literal"))
        }
        Token::NIL => { Primitive::Nil }
        _ => { panic!("{:?} is not a literal", lexeme.token) }
    };
    Box::new(LiteralExpression { lexeme: lexeme.clone(), literal })
}

fn tokenize_file(filename: &String) -> (Vec<Lexeme>, Result<(), ()>) {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
        String::new()
    });
    tokenize(file_contents.chars())
}

fn tokenize(data: Chars) -> (Vec<Lexeme>, Result<(), ()>) {
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
            writeln!(io::stderr(), "[line {line_number}] Error: Unexpected character: {c}").unwrap();
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

fn flush_token(tokens: &HashMap<Box<[char]>, Token>, mut chars: &mut Vec<char>, mut iterator: &mut CharIterator, lexemes: &mut Vec<Lexeme>) -> bool {
    if chars.len() > 0 {
        match tokens.get(chars.as_slice()) {
            None => {}
            Some(token) => {
                if token == &Token::COMMENT {
                    skip_until_next_line(&mut iterator, &mut chars);
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

fn read_string(c: char, iterator: &mut CharIterator, chars: &mut Vec<char>, lexemes: &mut Vec<Lexeme>) -> Result<(), ()> {
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

    #[test]
    fn test_2() {
        let (lexemes, _) = tokenize("55 * 89 / 30".chars());

        let expression = parse_lexemes(lexemes);
        assert_eq!("(/ (* 55.0 89.0) 30.0)", expression.to_string())
    }

    #[test]
    fn test_3() {
        let (lexemes, _) = tokenize("3 * -4 / 5".chars());

        let expression = parse_lexemes(lexemes);
        assert_eq!("(/ (* 3.0 (- 4.0)) 5.0)", expression.to_string())
    }
}