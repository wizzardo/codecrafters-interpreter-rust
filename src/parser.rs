use crate::expression::{BinaryExpression, BlockExpression, Expression, GroupExpression, LiteralExpression, NoopExpression, PrintExpression, UnaryMinusExpression, UnaryNotExpression, VariableDeclarationExpression, VariableExpression};
use crate::primitive::Primitive;
use crate::tokenizer::{Lexeme, Token};

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

pub fn parse_lexemes(lexemes: Vec<Lexeme>) -> Box<dyn Expression> {
    let mut iterator = LexemeIterator::from(lexemes);
    return parse(&mut iterator);
}

pub fn parse_statements(lexemes: Vec<Lexeme>) -> Vec<Box<dyn Expression>> {
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
            break;
        } else {
            eprintln!("unexpected token {:?}", lexeme.token);
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
                    break;
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
    let exp = BinaryExpression::new(lexeme, left, right);
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
            eprintln!("empty group expression");
            std::process::exit(65);
        }
    }

    let expression = parse(iterator);
    let end: Lexeme;
    match iterator.peek() {
        None => {
            eprintln!("unclosed group expression");
            std::process::exit(65);
        }
        Some(lexeme) => {
            if lexeme.token != Token::RIGHT_PAREN {
                eprintln!("{:?} != Token::RIGHT_PAREN", lexeme.token);
                std::process::exit(65);
            }

            end = lexeme.clone();
        }
    };
    iterator.advance();
    Box::new(GroupExpression::new(start, end, expression))
}

fn parse_block(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let start = iterator.peek().unwrap().clone();
    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_BRACE {
            eprintln!("empty block expression");
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
                eprintln!("unclosed block expression");
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
    Box::new(BlockExpression::new(start, end, expressions))
}

fn parse_unary_not(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    Box::new(UnaryNotExpression::new(lexeme, parse(iterator)))
}

fn parse_unary_minus(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    Box::new(UnaryMinusExpression::new(lexeme, parse_one(iterator)))
}

fn parse_print(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    Box::new(PrintExpression::new(lexeme, parse(iterator)))
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
            Box::new(VariableDeclarationExpression::new(lexeme, name, parse(iterator)))
        }
        Token::SEMICOLON => {
            Box::new(VariableDeclarationExpression::new(lexeme, name, Box::new(NoopExpression {})))
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
    Box::new(VariableExpression::new(lexeme.clone(), name))
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
    Box::new(LiteralExpression::new(lexeme.clone(), literal))
}