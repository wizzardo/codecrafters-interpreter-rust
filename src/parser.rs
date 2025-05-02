use crate::expression::{AnonymousFunctionCallExpression, BinaryExpression, BlockExpression, ClassDeclarationExpression, Expression, ForExpression, FunctionCallExpression, FunctionDefinitionExpression, GetFieldExpression, GroupExpression, IfExpression, LiteralExpression, MethodCallExpression, MethodDefinition, NoopExpression, PrintExpression, ReturnExpression, SetFieldExpression, UnaryMinusExpression, UnaryNotExpression, VariableDeclarationExpression, VariableExpression, WhileExpression};
use crate::primitive::Primitive;
use crate::tokenizer::{Lexeme, Token};

struct LexemeIterator {
    position: usize,
    limit: usize,
    lexemes: Vec<Lexeme>,
    in_class: bool,
    can_return_value: bool,
}

impl LexemeIterator {
    fn from(lexemes: Vec<Lexeme>) -> LexemeIterator {
        LexemeIterator {
            position: 0,
            limit: lexemes.len(),
            lexemes,
            in_class: false,
            can_return_value: true,
        }
    }
    fn peek(&self) -> Option<&Lexeme> {
        if self.position >= self.limit {
            None
        } else {
            Some(&self.lexemes[self.position])
        }
    }
    fn is(&self, token: Token) -> bool {
        self.is_n(0, token)
    }
    fn is_n(&self, i: usize, token: Token) -> bool {
        self.position + i < self.limit && self.lexemes[self.position + i].token == token
    }
    fn peek_n(&self, n: usize) -> Option<&Lexeme> {
        if self.position + n >= self.limit {
            None
        } else {
            Some(&self.lexemes[self.position + n])
        }
    }
    fn advance(&mut self) {
        self.position += 1;
    }
    fn is_in_class(&self) -> bool {
        self.in_class
    }
    fn set_in_class(&mut self, in_class: bool) {
        self.in_class = in_class;
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
        if iterator.is(Token::SEMICOLON) {
            iterator.advance();
            continue;
        }
        if iterator.peek().is_none() {
            break
        }

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
    'outer: while let Some(lexeme) = iterator.peek() {
        let expression = if lexeme.token.is_literal() {
            let expression = to_literal_expression(lexeme);
            iterator.advance();
            if iterator.is(Token::LEFT_PAREN) {
                eprintln!("Can only call functions and classes.");
                exit(70);
            }
            expression
        } else if lexeme.token == Token::LEFT_PAREN {
            if operands.len() > operations.len() {
                let e = operands.pop().unwrap();
                parse_anonymous_function_call(iterator, e)
            } else {
                let mut e = parse_group(iterator);
                while iterator.is(Token::LEFT_PAREN) {
                    e = parse_anonymous_function_call(iterator, e);
                }
                e
            }
        } else if lexeme.token == Token::IF {
            parse_if(iterator)
        } else if lexeme.token == Token::WHILE {
            parse_while(iterator)
        } else if lexeme.token == Token::FOR {
            parse_for(iterator)
        } else if lexeme.token == Token::LEFT_BRACE {
            parse_block(iterator, true)
        } else if lexeme.token == Token::BANG {
            parse_unary_not(iterator)
        } else if lexeme.token == Token::MINUS {
            parse_unary_minus(iterator)
        } else if lexeme.token == Token::PRINT {
            parse_print(iterator)
        } else if lexeme.token == Token::RETURN {
            parse_return(iterator)
        } else if lexeme.token == Token::VAR {
            parse_var(iterator)
        } else if lexeme.token == Token::CLASS {
            if operands.len() != 0 {
                eprintln!("Can only declare a class at the top level");
                exit(65);
            }
            return parse_class(iterator)
        } else if lexeme.token == Token::FUN {
            parse_function(iterator)
        } else if lexeme.token == Token::IDENTIFIER || lexeme.token == Token::THIS {
            if lexeme.token == Token::THIS && !iterator.is_in_class() {
                eprintln!("Can only use 'this' in a class");
                exit(65);
            }
            if let Some(next) = iterator.peek_n(1) {
                if next.token == Token::LEFT_PAREN {
                    parse_function_call(iterator)
                } else {
                    parse_identifier(iterator)
                }
            } else {
                parse_identifier(iterator)
            }
        } else if lexeme.token == Token::SEMICOLON {
            iterator.advance();
            break;
        } else {
            if operands.len() > operations.len() {
                break
            }
            eprintln!("unexpected token {:?}", lexeme.token);
            exit(65);
        };

        operands.push(expression);
        loop {
            match iterator.peek() {
                None => {
                    // return expression
                    break 'outer
                }
                Some(lexeme) => {
                    if lexeme.token == Token::SEMICOLON {
                        // iterator.advance();
                        break 'outer;
                    }
                    if lexeme.token == Token::DOT && iterator.is_n(1, Token::IDENTIFIER) && iterator.is_n(2, Token::EQUAL) {
                        let e = operands.pop().unwrap();
                        iterator.advance(); //skip dot
                        let name = iterator.peek().expect("expected a field name");
                        let lexeme = name.clone();
                        let field = name.src.iter().collect();
                        iterator.advance();
                        iterator.advance(); //skip =
                        let value = parse(iterator);
                        let x = Box::new(SetFieldExpression::new(lexeme, e, field, value));
                        operands.push(x);
                        break 'outer;
                    }
                    if lexeme.token == Token::DOT && iterator.is_n(1, Token::IDENTIFIER) && iterator.is_n(2, Token::LEFT_PAREN) {
                        let object = operands.pop().unwrap();
                        operands.push(parse_method_call(iterator, object));
                        continue;
                    }
                    if lexeme.token == Token::DOT && iterator.is_n(1, Token::IDENTIFIER) {
                        let e = operands.pop().unwrap();
                        operands.push(parse_field_access(iterator, e));
                        continue;
                    }
                    if lexeme.token == Token::LEFT_PAREN {
                        let e = operands.pop().unwrap();
                        operands.push(parse_anonymous_function_call(iterator, e));
                        break 'outer;
                    }
                    if lexeme.token.is_binary_operator() {
                        let lexeme = lexeme.clone();
                        operations.push(lexeme);
                        iterator.advance();
                        break
                    } else {
                        break 'outer;
                    }
                }
            }
        }
    }
    if operands.len() == 1 {
        return operands.into_iter().next().unwrap();
    } else if operands.len() == 0 {
        eprintln!("no expression found");
        exit(65);
    } else {
        create_binary(operands, operations)
    }
}

fn parse_method_call(iterator: &mut LexemeIterator, object: Box<dyn Expression>) -> Box<dyn Expression> {
    iterator.advance(); //skip dot
    let name = iterator.peek().expect("expected a method name");
    let lexeme = name.clone();
    let field = name.src.iter().collect();
    iterator.advance();
    iterator.advance(); //skip (

    let mut args = vec![];
    while let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            break;
        }

        let var = parse(iterator);
        args.push(var);

        if iterator.is(Token::COMMA) {
            iterator.advance();
        } else if !iterator.is(Token::RIGHT_PAREN) {
            eprintln!("expected ',' or ')' after argument name, but was {:?}", iterator.peek().map(|t| t.token));
            exit(65);
        }
    }

    if !iterator.is(Token::RIGHT_PAREN) {
        eprintln!("expected ) after function name");
        exit(65);
    }
    iterator.advance();

    Box::new(MethodCallExpression::new(lexeme, object, field, args))
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
            exit(65);
        }
    }

    let expression = parse(iterator);
    let end: Lexeme;
    match iterator.peek() {
        None => {
            eprintln!("unclosed group expression");
            exit(65);
        }
        Some(lexeme) => {
            if lexeme.token != Token::RIGHT_PAREN {
                eprintln!("{:?} != Token::RIGHT_PAREN", lexeme.token);
                exit(65);
            }

            end = lexeme.clone();
        }
    };
    iterator.advance();
    Box::new(GroupExpression::new(start, end, expression))
}

fn parse_if(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token != Token::LEFT_PAREN {
            eprintln!("condition block expected");
            exit(65);
        }
    }

    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            eprintln!("empty if condition");
            exit(65);
        }
    }

    let condition = parse(iterator);
    match iterator.peek() {
        None => {
            eprintln!("unclosed if condition");
            exit(65);
        }
        Some(lexeme) => {
            if lexeme.token != Token::RIGHT_PAREN {
                eprintln!("{:?} != Token::RIGHT_PAREN", lexeme.token);
                exit(65);
            }
        }
    };
    iterator.advance();

    let body = parse(iterator);
    if iterator.is(Token::SEMICOLON) {
        iterator.advance();
    }
    let else_body = match iterator.peek() {
        None => { None }
        Some(lexeme) => {
            if lexeme.token == Token::ELSE {
                iterator.advance();
                Some(parse(iterator))
            } else {
                None
            }
        }
    };
    if iterator.is(Token::SEMICOLON) {
        iterator.advance();
    }
    Box::new(IfExpression::new(condition, body, else_body))
}

fn parse_while(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token != Token::LEFT_PAREN {
            eprintln!("condition block expected");
            exit(65);
        }
    }

    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            eprintln!("empty while condition");
            exit(65);
        }
    }

    let condition = parse(iterator);
    match iterator.peek() {
        None => {
            eprintln!("unclosed while condition");
            exit(65);
        }
        Some(lexeme) => {
            if lexeme.token != Token::RIGHT_PAREN {
                eprintln!("{:?} != Token::RIGHT_PAREN", lexeme.token);
                exit(65);
            }
        }
    };
    iterator.advance();

    let body = parse(iterator);
    Box::new(WhileExpression::new(condition, body))
}

fn parse_for(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token != Token::LEFT_PAREN {
            eprintln!("condition block expected");
            exit(65);
        }
    }

    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            eprintln!("empty for condition");
            exit(65);
        }
    }


    let before = if let Some(l) = iterator.peek() {
        if l.token == Token::LEFT_BRACE {
            eprintln!("BlockExpression cannot be used as init for FOR expression");
            exit(65);
        }
        if l.token == Token::SEMICOLON {
            iterator.advance();
            None
        } else {
            Some(parse(iterator))
        }
    } else {
        eprintln!("unexpected end of FOR statement");
        exit(65);
    };

    if before.is_some() {
        if !iterator.is(Token::SEMICOLON) {
            eprintln!("should be ';' after for init, but was {:?}", iterator.peek().map(|t| t.token));
            exit(65);
        } else {
            iterator.advance();
        }
    }

    if let Some(l) = iterator.peek() {
        if l.token == Token::LEFT_BRACE {
            eprintln!("BlockExpression cannot be used as condition for FOR expression");
            exit(65);
        }
    }

    let condition = parse(iterator);

    if !iterator.is(Token::SEMICOLON) {
        eprintln!("should be ';' after for condition, but was {:?}", iterator.peek().map(|t| t.token));
        exit(65);
    } else {
        iterator.advance();
    }

    let after = if let Some(l) = iterator.peek() {
        if l.token == Token::LEFT_BRACE {
            eprintln!("BlockExpression cannot be used as after for FOR expression");
            exit(65);
        }
        if l.token == Token::RIGHT_PAREN {
            None
        } else {
            Some(parse(iterator))
        }
    } else {
        eprintln!("unexpected end of FOR statement");
        exit(65);
    };

    if iterator.is(Token::SEMICOLON) {
        iterator.advance();
    }

    match iterator.peek() {
        None => {
            eprintln!("unclosed while condition");
            exit(65);
        }
        Some(lexeme) => {
            if lexeme.token != Token::RIGHT_PAREN {
                eprintln!("{:?} != Token::RIGHT_PAREN", lexeme.token);
                exit(65);
            }
        }
    };
    iterator.advance();

    let body = parse(iterator);
    Box::new(ForExpression::new(before, condition, after, body))
}

fn parse_block(iterator: &mut LexemeIterator, create_subscope_on_execution: bool) -> Box<dyn Expression> {
    let start = iterator.peek().unwrap().clone();
    iterator.advance();
    if let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_BRACE {
            iterator.advance();
            return Box::new(BlockExpression::new(start.clone(), start, Vec::new(), create_subscope_on_execution))
        }
    }
    let mut expressions = vec![];
    let end: Lexeme;
    loop {
        if iterator.is(Token::SEMICOLON) {
            iterator.advance();
            continue
        }
        match iterator.peek() {
            None => {
                eprintln!("unclosed block expression");
                exit(65);
            }
            Some(lexeme) => {
                if lexeme.token == Token::RIGHT_BRACE {
                    end = lexeme.clone();
                    break;
                }
            }
        };

        let expression = parse(iterator);
        expressions.push(expression);
    }
    iterator.advance();
    Box::new(BlockExpression::new(start, end, expressions, create_subscope_on_execution))
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

fn parse_return(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();

    if let Some(l) = iterator.peek() {
        if l.token == Token::SEMICOLON {
            let end = l.clone();
            iterator.advance();
            return Box::new(ReturnExpression::new(lexeme, Box::new(LiteralExpression::new(end, Primitive::Nil))))
        }
    }
    if !iterator.can_return_value {
        eprintln!("cannot return value from here");
        exit(65);
    }

    Box::new(ReturnExpression::new(lexeme, parse(iterator)))
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
            exit(65);
        }
    }
}

fn parse_class(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    iterator.set_in_class(true);
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    let name = iterator.peek().expect("expected a class name");
    let name = name.src.iter().collect();
    iterator.advance();

    if !iterator.is(Token::LEFT_BRACE) {
        eprintln!("expected {{ after class name");
        exit(65);
    }
    iterator.advance();

    let mut methods = vec![];
    while iterator.is(Token::IDENTIFIER) && iterator.is_n(1, Token::LEFT_PAREN) {
        methods.push(parse_method(iterator));
    }

    if !iterator.is(Token::RIGHT_BRACE) {
        eprintln!("expected }} after class name");
        exit(65);
    }
    iterator.advance();
    iterator.set_in_class(false);
    Box::new(ClassDeclarationExpression::new(lexeme, name, methods))
}

fn parse_function(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    let parent_can_return_value = iterator.can_return_value;
    iterator.can_return_value = true;
    let (name, args, body) = parse_function_definition(iterator);
    iterator.can_return_value = parent_can_return_value;
    Box::new(FunctionDefinitionExpression::new(lexeme, name, args, body))
}

fn parse_method(iterator: &mut LexemeIterator) -> MethodDefinition {
    let lexeme = iterator.peek().unwrap().clone();

    let name = iterator.peek().expect("expected a function name");
    let name = name.src.iter().collect::<String>();
    if name.eq("init") {
        iterator.can_return_value = false;
    }
    let (name, args, body) = parse_function_definition(iterator);
    iterator.can_return_value = true;
    MethodDefinition::new(lexeme, name, args, body)
}

fn parse_function_definition(iterator: &mut LexemeIterator) -> (String, Vec<Box<str>>, Box<dyn Expression>) {
    let name = iterator.peek().expect("expected a function name");
    let name = name.src.iter().collect();
    iterator.advance();

    if !iterator.is(Token::LEFT_PAREN) {
        eprintln!("expected ( after function name");
        exit(65);
    }
    iterator.advance();

    let mut args = vec![];
    while let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            break;
        }

        let var = l.src.iter().collect();
        args.push(var);
        iterator.advance();

        if iterator.is(Token::COMMA) {
            iterator.advance();
        } else if !iterator.is(Token::RIGHT_PAREN) {
            eprintln!("expected ',' or ')' after argument name");
            exit(65);
        }
    }

    if !iterator.is(Token::RIGHT_PAREN) {
        eprintln!("expected ) after function name");
        exit(65);
    }
    iterator.advance();

    if !iterator.is(Token::LEFT_BRACE) {
        eprintln!("expected {{ after function arguments");
        exit(65);
    }
    let body = parse_block(iterator, false);
    (name, args, body)
}

fn parse_identifier(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    iterator.advance();
    let name = lexeme.src.iter().collect();
    Box::new(VariableExpression::new(lexeme.clone(), name))
}

fn parse_field_access(iterator: &mut LexemeIterator, object: Box<dyn Expression>) -> Box<dyn Expression> {
    iterator.advance();
    let lexeme = iterator.peek().unwrap().clone();
    let name = iterator.peek().expect("expected a field name");
    let field = name.src.iter().collect();
    iterator.advance();

    Box::new(GetFieldExpression::new(lexeme, object, field))
}

fn parse_function_call(iterator: &mut LexemeIterator) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    let name = lexeme.src.iter().collect();
    iterator.advance();

    if !iterator.is(Token::LEFT_PAREN){
        eprintln!("expected ( after function name");
        exit(65);
    }
    iterator.advance();

    let mut args = vec![];
    while let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            break;
        }

        let var = parse(iterator);
        args.push(var);

        if iterator.is(Token::COMMA) {
            iterator.advance();
        } else if !iterator.is(Token::RIGHT_PAREN) {
            eprintln!("expected ',' or ')' after argument name, but was {:?}", iterator.peek().map(|t| t.token));
            exit(65);
        }
    }

    if !iterator.is(Token::RIGHT_PAREN){
        eprintln!("expected ) after function name");
        exit(65);
    }
    iterator.advance();

    Box::new(FunctionCallExpression::new(lexeme.clone(), name, args))
}

fn parse_anonymous_function_call(iterator: &mut LexemeIterator, fun: Box<dyn Expression>) -> Box<dyn Expression> {
    let lexeme = iterator.peek().unwrap().clone();
    if !iterator.is(Token::LEFT_PAREN){
        eprintln!("expected ( after function name");
        exit(65);
    }
    iterator.advance();

    let mut args = vec![];
    while let Some(l) = iterator.peek() {
        if l.token == Token::RIGHT_PAREN {
            break;
        }

        let var = parse(iterator);
        args.push(var);

        if iterator.is(Token::COMMA) {
            iterator.advance();
        } else if !iterator.is(Token::RIGHT_PAREN) {
            eprintln!("expected ',' or ')' after argument name, but was {:?}", iterator.peek().map(|t| t.token));
            exit(65);
        }
    }

    if !iterator.is(Token::RIGHT_PAREN){
        eprintln!("expected ) after function name");
        exit(65);
    }
    iterator.advance();

    Box::new(AnonymousFunctionCallExpression::new(lexeme.clone(), fun, args))
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

fn exit(code: i32) -> ! {
    std::process::exit(code)
}