use crate::value::Value;
use crate::primitive::Primitive;
use crate::scope::Scope;
use crate::tokenizer::{Lexeme, Token};

pub trait Expression {
    fn to_string(&self) -> String;
    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String>;
    fn to_variable(&self) -> Option<String> {
        None
    }
}

#[allow(unused)]
pub struct LiteralExpression {
    lexeme: Lexeme,
    literal: Primitive,
}

impl LiteralExpression {
    pub fn new(lexeme: Lexeme, literal: Primitive) -> Self {
        LiteralExpression { lexeme, literal }
    }
}

#[allow(unused)]
pub struct GroupExpression {
    start: Lexeme,
    end: Lexeme,
    expression: Box<dyn Expression>,
}

impl GroupExpression {
    pub fn new(start: Lexeme, end: Lexeme, expression: Box<dyn Expression>) -> Self {
        GroupExpression { start, end, expression }
    }
}

#[allow(unused)]
pub struct IfExpression {
    condition: Box<dyn Expression>,
    body: Box<dyn Expression>,
    else_body: Option<Box<dyn Expression>>,
}

impl IfExpression {
    pub fn new(condition: Box<dyn Expression>, body: Box<dyn Expression>, else_body: Option<Box<dyn Expression>>) -> Self {
        IfExpression { condition, body, else_body: else_body }
    }
}

#[allow(unused)]
pub struct BlockExpression {
    start: Lexeme,
    end: Lexeme,
    expressions: Vec<Box<dyn Expression>>,
}

impl BlockExpression {
    pub fn new(start: Lexeme, end: Lexeme, expressions: Vec<Box<dyn Expression>>) -> Self {
        BlockExpression { start, end, expressions }
    }
}

#[allow(unused)]
pub struct UnaryNotExpression {
    lexeme: Lexeme,
    expression: Box<dyn Expression>,
}

impl UnaryNotExpression {
    pub fn new(lexeme: Lexeme, expression: Box<dyn Expression>) -> Self {
        UnaryNotExpression { lexeme, expression }
    }
}

#[allow(unused)]
pub struct UnaryMinusExpression {
    lexeme: Lexeme,
    expression: Box<dyn Expression>,
}

impl UnaryMinusExpression {
    pub fn new(lexeme: Lexeme, expression: Box<dyn Expression>) -> Self {
        UnaryMinusExpression { lexeme, expression }
    }
}

#[allow(unused)]
pub struct PrintExpression {
    lexeme: Lexeme,
    expression: Box<dyn Expression>,
}

impl PrintExpression {
    pub fn new(lexeme: Lexeme, expression: Box<dyn Expression>) -> Self {
        PrintExpression { lexeme, expression }
    }
}

#[allow(unused)]
pub struct VariableDeclarationExpression {
    lexeme: Lexeme,
    name: String,
    expression: Box<dyn Expression>,
}

impl VariableDeclarationExpression {
    pub fn new(lexeme: Lexeme, name: String, expression: Box<dyn Expression>) -> Self {
        VariableDeclarationExpression { lexeme, name, expression }
    }
}

#[allow(unused)]
pub struct VariableExpression {
    lexeme: Lexeme,
    name: String,
}

impl VariableExpression {
    pub fn new(lexeme: Lexeme, name: String) -> Self {
        VariableExpression { lexeme, name }
    }
}

#[allow(unused)]
pub struct BinaryExpression {
    lexeme: Lexeme,
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl BinaryExpression {
    pub fn new(lexeme: Lexeme, left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        BinaryExpression { lexeme, left, right }
    }
}

pub struct NoopExpression {}

impl Expression for NoopExpression {
    fn to_string(&self) -> String {
        format!("noop")
    }

    fn evaluate(&self, _scope: &mut Scope) -> Result<Value, String> {
        Ok(Value::Primitive(Primitive::Nil))
    }
}

impl Expression for LiteralExpression {
    fn to_string(&self) -> String {
        self.literal.to_string()
    }

    fn evaluate(&self, _scope: &mut Scope) -> Result<Value, String> {
        Ok(Value::Primitive(self.literal.clone()))
    }
}

impl Expression for GroupExpression {
    fn to_string(&self) -> String {
        format!("(group {})", self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        self.expression.evaluate(scope)
    }
}

impl Expression for IfExpression {
    fn to_string(&self) -> String {
        format!("(if ({}) {})", self.condition.to_string(), self.body.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let check = self.condition.evaluate(scope)?;
        match check {
            Value::Primitive(p) => {
                match p {
                    Primitive::Boolean(b) => {
                        if b {
                            self.body.evaluate(scope)
                        } else if let Some(e) = &self.else_body {
                            e.evaluate(scope)
                        } else {
                            Ok(Value::Primitive(Primitive::Nil))
                        }
                    }
                    it => { Err(format!("if condition evaluated to '{it:?}' that is not a boolean")) }
                }
            }
        }
    }
}

impl Expression for BlockExpression {
    fn to_string(&self) -> String {
        let expressions: Vec<String> = self.expressions.iter().map(|x| x.to_string()).collect();
        format!("(block {})", expressions.join(", "))
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let mut value = Value::Primitive(Primitive::Nil);

        scope.push_scope();
        for x in &self.expressions {
            value = x.evaluate(scope)?;
        }
        scope.pop_scope();
        return Ok(value);
    }
}

impl Expression for UnaryNotExpression {
    fn to_string(&self) -> String {
        format!("(! {})", self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
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

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
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

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
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

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let value = self.expression.evaluate(scope)?;
        scope.define(self.name.clone(), value);
        Ok(Value::Primitive(Primitive::Nil))
    }
}

impl Expression for VariableExpression {
    fn to_string(&self) -> String {
        format!("var {}", self.name)
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
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

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        if self.lexeme.token == Token::EQUAL {
            return match self.left.to_variable() {
                Some(variable) => {
                    let value = self.right.evaluate(scope)?;
                    scope.set(variable, value.clone());
                    Ok(value)
                }
                None => {
                    Err(format!("Cannot assign not a variable"))
                    // std::process::exit(65);
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