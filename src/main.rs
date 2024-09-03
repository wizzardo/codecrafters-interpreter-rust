use std::collections::{HashMap, HashSet};
use std::env;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::io::{self, Write};
use std::str::Chars;
use std::any::Any;

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

    fn is_binary_operator(&self) -> bool {
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
        "evaluate" => {
            let (lexemes, result) = tokenize_file(filename);
            if result.is_err() {
                std::process::exit(65);
            }
            let expression = parse_lexemes(lexemes);
            let mut scope = HashMap::new();
            let result = expression.evaluate(&mut scope);
            match result {
                Ok(value) => {
                    match value {
                        Value::Primitive(v) => {
                            match v {
                                Primitive::Number(v) => { println!("{}", v) }
                                Primitive::String(v) => { println!("{}", v) }
                                Primitive::Boolean(v) => { println!("{}", v) }
                                Primitive::Nil => { println!("nil") }
                            }
                        }
                    }
                }
                Err(_) => {
                    std::process::exit(70);
                }
            }
        }
        "run" => {
            let (lexemes, result) = tokenize_file(filename);
            if result.is_err() {
                std::process::exit(65);
            }
            let mut scope = HashMap::new();
            let statements = parse_statements(lexemes);
            let mut _result;
            for x in statements {
                eprintln!("evaluating {}", x.to_string());
                _result = match x.evaluate(&mut scope) {
                    Ok(v) => { v }
                    Err(s) => {
                        eprintln!("{s}");
                        std::process::exit(70);
                    }
                };
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}

trait Expression: Any {
    fn to_string(&self) -> String;
    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String>;
    fn to_variable(&self) -> Option<String> {
        None
    }
}

#[derive(Clone)]
enum Value {
    Primitive(Primitive),
}

#[allow(unused)]
impl Value {
    fn to_string(&self) -> String {
        match self { Value::Primitive(p) => { p.to_string() } }
    }
}

#[allow(unused)]
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
struct BlockExpression {
    start: Lexeme,
    end: Lexeme,
    expressions: Vec<Box<dyn Expression>>,
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
struct PrintExpression {
    lexeme: Lexeme,
    expression: Box<dyn Expression>,
}

#[allow(unused)]
struct VariableDeclarationExpression {
    lexeme: Lexeme,
    name: String,
    expression: Box<dyn Expression>,
}

#[allow(unused)]
struct VariableExpression {
    lexeme: Lexeme,
    name: String,
}

#[allow(unused)]
struct BinaryExpression {
    lexeme: Lexeme,
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

struct NoopExpression {}

impl Expression for NoopExpression {
    fn to_string(&self) -> String {
        format!("noop")
    }

    fn evaluate(&self, _scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        Ok(Value::Primitive(Primitive::Nil))
    }
}

impl Expression for LiteralExpression {
    fn to_string(&self) -> String {
        self.literal.to_string()
    }

    fn evaluate(&self, _scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        Ok(Value::Primitive(self.literal.clone()))
    }
}

impl Expression for GroupExpression {
    fn to_string(&self) -> String {
        format!("(group {})", self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        self.expression.evaluate(scope)
    }
}

impl Expression for BlockExpression {
    fn to_string(&self) -> String {
        let expressions: Vec<String> = self.expressions.iter().map(|x| x.to_string()).collect();
        format!("(block {})", expressions.join(", "))
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        let mut value = Value::Primitive(Primitive::Nil);
        for x in &self.expressions {
            value = x.evaluate(scope)?;
        }
        return Ok(value);
    }
}

impl Expression for UnaryNotExpression {
    fn to_string(&self) -> String {
        format!("(! {})", self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        let value = self.expression.evaluate(scope)?;

        match value {
            Value::Primitive(v) => {
                match v {
                    Primitive::Boolean(b) => {
                        Ok(Value::Primitive(Primitive::Boolean(!b)))
                    }
                    Primitive::Nil => {
                        Ok(Value::Primitive(Primitive::Boolean(true)))
                    }
                    Primitive::Number(n) => {
                        Ok(Value::Primitive(Primitive::Boolean(n == 0.0)))
                    }
                    Primitive::String(s) => {
                        Ok(Value::Primitive(Primitive::Boolean(!s.is_empty())))
                    }
                }
            }
        }
    }
}

impl Expression for UnaryMinusExpression {
    fn to_string(&self) -> String {
        format!("(- {})", self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        let value = self.expression.evaluate(scope)?;

        match value {
            Value::Primitive(v) => {
                match v {
                    Primitive::Number(n) => {
                        Ok(Value::Primitive(Primitive::Number(-n)))
                    }
                    p => { Err(format!("Cannot apply unary minus to {}", p.to_string())) }
                }
            }
        }
    }
}

impl Expression for PrintExpression {
    fn to_string(&self) -> String {
        format!("print {}", self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        let value = self.expression.evaluate(scope)?;

        match value {
            Value::Primitive(v) => {
                match v {
                    Primitive::Number(n) => { println!("{}", n); }
                    Primitive::String(s) => { println!("{}", s); }
                    Primitive::Boolean(b) => { println!("{}", b); }
                    Primitive::Nil => { println!("nil"); }
                }
            }
        }
        Ok(Value::Primitive(Primitive::Nil))
    }
}

impl Expression for VariableDeclarationExpression {
    fn to_string(&self) -> String {
        format!("var {} = {}", self.name, self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        let value = self.expression.evaluate(scope)?;
        scope.insert(self.name.clone(), value);
        Ok(Value::Primitive(Primitive::Nil))
    }
}

impl Expression for VariableExpression {
    fn to_string(&self) -> String {
        format!("var {}", self.name)
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        match scope.get(&self.name) {
            None => {
                Err(format!("Variable {} not found", self.name))
            }
            Some(v) => {
                Ok(v.clone())
            }
        }
    }

    fn to_variable(&self) -> Option<String> {
        Some(self.name.clone())
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
            Token::EQUAL => { "=" }
            t => { panic!("{:?} is not an action for binary expression", t) }
        };
        format!("({} {} {})", action, self.left.to_string(), self.right.to_string())
    }

    fn evaluate(&self, scope: &mut HashMap<String, Value>) -> Result<Value, String> {
        if self.lexeme.token == Token::EQUAL {
            return match self.left.to_variable() {
                Some(variable) => {
                    let value = self.right.evaluate(scope)?;
                    scope.insert(variable, value.clone());
                    Ok(value)
                }
                None => {
                    // Err(format!("Cannot assign not a variable"));
                    std::process::exit(65);
                }
            };
        }
        let left = self.left.evaluate(scope)?;
        let right = self.right.evaluate(scope)?;

        match (left, right) {
            (Value::Primitive(l), Value::Primitive(r)) => {
                match (l, r) {
                    (Primitive::Number(l), Primitive::Number(r)) => {
                        match self.lexeme.token {
                            Token::STAR => { Ok(Value::Primitive(Primitive::Number(l * r))) }
                            Token::MINUS => { Ok(Value::Primitive(Primitive::Number(l - r))) }
                            Token::PLUS => { Ok(Value::Primitive(Primitive::Number(l + r))) }
                            Token::SLASH => { Ok(Value::Primitive(Primitive::Number(l / r))) }
                            Token::GREATER => { Ok(Value::Primitive(Primitive::Boolean(l > r))) }
                            Token::GREATER_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l >= r))) }
                            Token::LESS => { Ok(Value::Primitive(Primitive::Boolean(l < r))) }
                            Token::LESS_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l <= r))) }
                            Token::EQUAL_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l == r))) }
                            Token::BANG_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l != r))) }
                            _ => { Err(format!("Cannot apply {:?} expression to number and number", self.lexeme.token)) }
                        }
                    }
                    (Primitive::String(l), Primitive::String(r)) => {
                        match self.lexeme.token {
                            Token::PLUS => { Ok(Value::Primitive(Primitive::String(format!("{l}{r}")))) }
                            Token::EQUAL_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l == r))) }
                            Token::BANG_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l != r))) }
                            _ => { Err(format!("Cannot apply {:?} expression to String and String", self.lexeme.token)) }
                        }
                    }
                    (Primitive::Boolean(l), Primitive::Boolean(r)) => {
                        match self.lexeme.token {
                            Token::EQUAL_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l == r))) }
                            Token::BANG_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(l != r))) }
                            _ => { Err(format!("Cannot apply {:?} expression to boolean and boolean", self.lexeme.token)) }
                        }
                    }
                    (l, r) => {
                        match self.lexeme.token {
                            Token::EQUAL_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(false))) }
                            Token::BANG_EQUAL => { Ok(Value::Primitive(Primitive::Boolean(true))) }
                            _ => { Err(format!("Cannot apply {:?} expression to {} and {}", self.lexeme.token, l.to_string(), r.to_string())) }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
enum Primitive {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Primitive {
    fn to_string(&self) -> String {
        match &self {
            Primitive::Number(value) => {
                let number_str = value.to_string();
                if number_str.contains('.') {
                    number_str
                } else {
                    let mut x: Vec<char> = number_str.chars().collect();
                    x.push('.');
                    x.push('0');
                    x.iter().collect()
                }
            }
            Primitive::String(it) => { it.clone() }
            Primitive::Boolean(it) => { it.to_string() }
            Primitive::Nil => { "nil".to_string() }
        }
    }
}

fn parse_lexemes(lexemes: Vec<Lexeme>) -> Box<dyn Expression> {
    let mut iterator = LexemeIterator::from(lexemes);
    return parse(&mut iterator);
}

fn parse_statements(lexemes: Vec<Lexeme>) -> Vec<Box<dyn Expression>> {
    let mut iterator = LexemeIterator::from(lexemes);
    let mut statements: Vec<Box<dyn Expression>> = vec![];
    loop {
        let exp = parse(&mut iterator);
        statements.push(exp);
        if iterator.peek().is_none() {
            break;
        }
    }
    return statements;
}

fn parse(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let mut operands: Vec<Box<dyn Expression>> = vec![];
    let mut operations: Vec<Lexeme> = vec![];
    while let Some(lexeme) = iterator.peek() {
        let expression = if lexeme.token.is_literal() {
            let expression = to_literal_expression(lexeme);
            iterator.advance();
            expression
        } else if lexeme.token == Token::LEFT_PAREN {
            parse_group(iterator)
        } else if lexeme.token == Token::LEFT_BRACE {
            parse_block(iterator)
        } else if lexeme.token == Token::BANG {
            parse_unary_not(iterator)
        } else if lexeme.token == Token::MINUS {
            parse_unary_minus(iterator)
        } else if lexeme.token == Token::PRINT {
            parse_print(iterator)
        } else if lexeme.token == Token::VAR {
            parse_var(iterator)
        } else if lexeme.token == Token::IDENTIFIER {
            parse_identifier(iterator)
        } else if lexeme.token == Token::SEMICOLON {
            iterator.advance();
            if operands.is_empty() {
                std::process::exit(65);
            }
            break
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
                if lexeme.token == Token::SEMICOLON {
                    iterator.advance();
                    break
                }
                if lexeme.token.is_binary_operator() {
                    let lexeme = lexeme.clone();
                    operations.push(lexeme);
                    iterator.advance();
                    // let right = parse(iterator);
                    // break
                    // return Box::new(BinaryExpression { lexeme, left, right });
                } else {
                    // return expression;
                    break;
                }
            }
        }
    }
    if operands.len() == 1 {
        return operands.into_iter().next().unwrap();
    } else if operands.len() == 0 {
        panic!("no expression found")
    } else {
        create_binary(operands, operations)
    }
}

fn create_binary(mut operands: Vec<Box<dyn Expression>>, mut operations: Vec<Lexeme>) -> Box<dyn Expression> {
    loop {
        let option = operations.iter().enumerate().find(|(_, it)| { it.token == Token::STAR || it.token == Token::SLASH });
        match option {
            None => { break; }
            Some((i, _)) => {
                reduce_operation(&mut operands, &mut operations, i);
            }
        }
    };
    loop {
        let option = operations.iter().enumerate().find(|(_, it)| { it.token == Token::PLUS || it.token == Token::MINUS });
        match option {
            None => { break; }
            Some((i, _)) => {
                reduce_operation(&mut operands, &mut operations, i);
            }
        }
    };
    loop {
        let option = operations.iter().enumerate().find(|(_, it)| {
            it.token == Token::LESS
                || it.token == Token::LESS_EQUAL
                || it.token == Token::GREATER
                || it.token == Token::GREATER_EQUAL
                || it.token == Token::BANG_EQUAL
                || it.token == Token::EQUAL_EQUAL
        });
        match option {
            None => { break; }
            Some((i, _)) => {
                reduce_operation(&mut operands, &mut operations, i);
            }
        }
    };
    loop {
        if operations.is_empty() {
            break;
        }

        // should be '=' as last operations, reverse order here
        let i = operations.len() - 1;
        reduce_operation(&mut operands, &mut operations, i);
    };

    operands.remove(0)
}

fn reduce_operation(operands: &mut Vec<Box<dyn Expression>>, operations: &mut Vec<Lexeme>, i: usize) {
    let lexeme = operations.remove(i);
    let left = operands.remove(i);
    let right = operands.remove(i);
    let exp = BinaryExpression { lexeme, left, right };
    operands.insert(i, Box::new(exp));
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

fn parse_block(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let start = iterator.peek().unwrap().clone();
    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_BRACE {
            writeln!(io::stderr(), "empty block expression").unwrap();
            std::process::exit(65);
        }
    }
    let mut expressions = vec![];
    let end: Lexeme;
    loop {
        let expression = parse(iterator);
        expressions.push(expression);
        match iterator.peek() {
            None => {
                writeln!(io::stderr(), "unclosed block expression").unwrap();
                std::process::exit(65);
            }
            Some(lexeme) => {
                if lexeme.token == Token::RIGHT_BRACE {
                    end = lexeme.clone();
                    break;
                }
            }
        };
    }
    iterator.advance();
    Box::new(BlockExpression { start, end, expressions })
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

fn parse_print(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    Box::new(PrintExpression { lexeme, expression: parse(iterator) })
}

fn parse_var(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    let name = iterator.peek().expect("expected a variable name");
    let name = name.src.iter().collect();
    iterator.advance();
    match iterator.peek().expect("expected a variable name").token {
        Token::EQUAL => {
            iterator.advance();
            Box::new(VariableDeclarationExpression { lexeme, name, expression: parse(iterator) })
        }
        Token::SEMICOLON => {
            Box::new(VariableDeclarationExpression { lexeme, name, expression: Box::new(NoopExpression {}) })
        }
        _ => {
            println!("expected '=' or ';' after variable name");
            std::process::exit(65);
        }
    }
}

fn parse_identifier(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    let name = lexeme.src.iter().collect();
    Box::new(VariableExpression { lexeme: lexeme.clone(), name })
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

    #[test]
    fn test_4() {
        let (lexemes, _) = tokenize("1 + 2 * 3".chars());

        let expression = parse_lexemes(lexemes);
        assert_eq!("(+ 1.0 (* 2.0 3.0))", expression.to_string())
    }

    #[test]
    fn test_5() {
        let (lexemes, _) = tokenize("76 * -30".chars());

        let expression = parse_lexemes(lexemes);
        assert_eq!("(* 76.0 (- 30.0))", expression.to_string())
    }

    #[test]
    fn test_6() {
        let (lexemes, _) = tokenize("-(-22 + 29) * (90 * 48) / (84 + 13)".chars());

        let expression = parse_lexemes(lexemes);
        assert_eq!("(/ (* (- (group (+ (- 22.0) 29.0))) (group (* 90.0 48.0))) (group (+ 84.0 13.0)))", expression.to_string())
    }

    #[test]
    fn test_7() {
        let (lexemes, _) = tokenize("(90 - 94)".chars());

        let expression = parse_lexemes(lexemes);
        assert_eq!("(group (- 90.0 94.0))", expression.to_string())
    }

    #[test]
    fn test_evaluate_1() {
        let (lexemes, _) = tokenize("78 == \"78\"".chars());

        let expression = parse_lexemes(lexemes);
        let mut scope = HashMap::new();
        let result = expression.evaluate(&mut scope).unwrap();
        match result {
            Value::Primitive(v) => {
                assert_eq!("false", v.to_string())
            }
        }
    }

    #[test]
    fn test_evaluate_2() {
        let (lexemes, _) = tokenize("(96 * 2 + 48 * 2) / (2)".chars());

        let expression = parse_lexemes(lexemes);
        let mut scope = HashMap::new();
        let result = expression.evaluate(&mut scope).unwrap();
        match result {
            Value::Primitive(v) => {
                assert_eq!("144.0", v.to_string())
            }
        }
    }

    #[test]
    fn test_evaluate_3() {
        let (lexemes, _) = tokenize("var foo = 1".chars());

        let expression = parse_lexemes(lexemes);
        let mut scope = HashMap::new();
        let _ = expression.evaluate(&mut scope).unwrap();
        assert_eq!("1.0", scope.get("foo").expect("expect variable to be there").to_string());
    }
}