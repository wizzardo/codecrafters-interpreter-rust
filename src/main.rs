use scope::Scope;
use value::Value;
use std::env;
use std::time::SystemTime;
use crate::expression::NativeFunctionExpression;

mod tokenizer;
mod parser;
mod scope;
mod expression;
mod primitive;
mod value;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];


    match command.as_str() {
        "tokenize" => {
            let (lexemes, result) = tokenizer::tokenize_file(filename);

            for l in lexemes {
                println!("{}", l)
            }
            println!("EOF  null");

            if result.is_err() {
                std::process::exit(65);
            }
        }
        "parse" => {
            let (lexemes, result) = tokenizer::tokenize_file(filename);
            if result.is_err() {
                std::process::exit(65);
            }
            let expression = parser::parse_lexemes(lexemes);
            println!("{}", expression.to_string())
        }
        "evaluate" => {
            let (lexemes, result) = tokenizer::tokenize_file(filename);
            if result.is_err() {
                std::process::exit(65);
            }
            let expression = parser::parse_lexemes(lexemes);
            let mut scope = Scope::new();
            let result = expression.evaluate(&mut scope);
            match result {
                Ok(value) => {
                    println!("{}", value.to_string())
                }
                Err(_) => {
                    std::process::exit(70);
                }
            }
        }
        "run" => {
            let (lexemes, result) = tokenizer::tokenize_file(filename);
            if result.is_err() {
                std::process::exit(65);
            }
            let mut scope = Scope::new();

            let clock: Box<dyn Fn(Vec<Value>) -> Result<Value, String>> = Box::new(move |_| {
                let now = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs();
                Ok(Value::from_number(now as f64))
            });
            scope.define("clock".to_string(), Value::from_function(Box::new(NativeFunctionExpression::new("clock".to_string(), clock))));

            let statements = parser::parse_statements(lexemes);
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
            eprintln!("Unknown command: {command}");
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{parse_lexemes, parse_statements};
    use crate::tokenizer::{get_tokens_map, tokenize, Token};
    use value::Value;
    use crate::expression::NativeFunctionExpression;

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
        let mut scope = Scope::new();
        let result = expression.evaluate(&mut scope).unwrap();
        match result {
            Value::Primitive(v) => {
                assert_eq!("false", v.to_string())
            }
            _ => { assert!(false) }
        }
    }

    #[test]
    fn test_evaluate_2() {
        let (lexemes, _) = tokenize("(96 * 2 + 48 * 2) / (2)".chars());

        let expression = parse_lexemes(lexemes);
        let mut scope = Scope::new();
        let result = expression.evaluate(&mut scope).unwrap();
        match result {
            Value::Primitive(v) => {
                assert_eq!("144", v.to_string())
            }
            _ => { assert!(false) }
        }
    }

    #[test]
    fn test_evaluate_3() {
        let (lexemes, _) = tokenize("var foo = 1".chars());

        let expression = parse_lexemes(lexemes);
        let mut scope = Scope::new();
        let _ = expression.evaluate(&mut scope).unwrap();
        assert_eq!("1", scope.get(&"foo".to_string()).expect("expect variable to be there").borrow().to_string());
    }

    #[test]
    fn test_run_1() {
        let (lexemes, _) = tokenize(r##"
            var quz = (17 * 25) - 87;
            {
                var bar = "hello" + "13";
                print bar;
            }
            print quz;
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        for exp in expressions {
            exp.evaluate(&mut scope).unwrap();
        }
    }

    #[test]
    fn test_run_2() {
        let (lexemes, _) = tokenize("
            var a = 1;
            {
               a = 2;
            }
            print a;
        ".chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        for exp in expressions {
            exp.evaluate(&mut scope).unwrap();
        }
        assert_eq!("2", scope.get(&"a".to_string()).expect("expect variable to be there").borrow().to_string());
    }

    #[test]
    fn test_run_if() {
        let (lexemes, _) = tokenize(r##"
            var a = 1;
            if (true)
                a = 2;
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        for exp in expressions {
            exp.evaluate(&mut scope).unwrap();
        }
        assert_eq!("2", scope.get(&"a".to_string()).expect("expect variable to be there").borrow().to_string());
    }

    #[test]
    fn test_run_if_else() {
        let (lexemes, _) = tokenize(r##"
            var a = 1;
            if (false)
                a = 2;
            else
                a = 3;
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        for exp in expressions {
            exp.evaluate(&mut scope).unwrap();
        }
        assert_eq!("3", scope.get(&"a".to_string()).expect("expect variable to be there").borrow().to_string());
    }

    #[test]
    fn test_run_for_1() {
        let (lexemes, _) = tokenize(r##"
            var a = 0;
            for (var i = 0; i < 3;) {
                i = i + 1;
                a = i;
            }
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        for exp in expressions {
            exp.evaluate(&mut scope).unwrap();
        }
        assert_eq!("3", scope.get(&"a".to_string()).expect("expect variable to be there").borrow().to_string());
    }

    #[test]
    fn test_run_for_2() {
        let (lexemes, _) = tokenize(r##"
            var a = 0;
            var i = 0;
            for (; i < 3; i = i + 1;) {
                a = i;
            }
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        for exp in expressions {
            exp.evaluate(&mut scope).unwrap();
        }
        assert_eq!("2", scope.get(&"a".to_string()).expect("expect variable to be there").borrow().to_string());
    }

    #[test]
    fn test_run_for_3() {
        let (lexemes, _) = tokenize(r##"
            var a = 0;
            var i = 0;
            for (; i < 3;) {
                i = i + 1;
                a = i;
            }
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        for exp in expressions {
            exp.evaluate(&mut scope).unwrap();
        }
        assert_eq!("3", scope.get(&"a".to_string()).expect("expect variable to be there").borrow().to_string());
    }

    #[test]
    fn test_call_function_1() {
        let (lexemes, _) = tokenize(r##"
            print clock();
        "##.chars());

        let expressions = parse_statements(lexemes);
        assert_eq!(1, expressions.len());
        assert_eq!("print clock()", expressions[0].to_string());
        let mut scope = Scope::new();

        let now = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs() as f64;

        let clock: Box<dyn Fn(Vec<Value>) -> Result<Value, String>> = Box::new(move |_| {
            Ok(Value::from_number(now))
        });
        scope.define("clock".to_string(), Value::from_function(Box::new(NativeFunctionExpression::new("clock".to_string(), clock))));

        let fun = scope.get(&"clock".to_string()).expect("expect variable to be there");
        let fun = match &(*fun.borrow()) {
            Value::Function(fun) => {
                fun.clone()
            }
            _ => { panic!() }
        };

        assert_eq!(format!("{now}"), fun.evaluate(vec![]).unwrap().to_string());
    }

    #[test]
    fn test_custom_function_1() {
        let (lexemes, _) = tokenize(r##"
            fun f1(a) { a*2 }
            f1(2);
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        let mut result = Value::from_number(0.0);
        for exp in expressions {
            result = exp.evaluate(&mut scope).unwrap();
        }

        assert_eq!(format!("4"), result.to_string());
    }

    #[test]
    fn test_custom_function_2() {
        let (lexemes, _) = tokenize(r##"
            fun multiplier(a) { 
                fun f(b) { a*b }
                return f;
            }
            var x2 = multiplier(2);
            x2(3);
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        let mut result = Value::from_number(0.0);
        for exp in expressions {
            result = exp.evaluate(&mut scope).unwrap();
        }

        assert_eq!(format!("6"), result.to_string());
    }

    #[test]
    fn test_custom_function_3() {
        let (lexemes, _) = tokenize(r##"
            fun returnArg(arg) {
              return arg;
            }
            
            fun returnFunCallWithArg(func, arg) {
              return returnArg(func)(arg);
            }
            
            fun doubleArg(arg) {
              return arg*2;
            }
            
            returnFunCallWithArg(doubleArg, 3);
        "##.chars());

        let expressions = parse_statements(lexemes);
        let mut scope = Scope::new();
        let mut result = Value::from_number(0.0);
        for exp in expressions {
            result = exp.evaluate(&mut scope).unwrap();
        }

        assert_eq!(format!("6"), result.to_string());
    }
}