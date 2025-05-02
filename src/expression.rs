use std::any::Any;
use std::cell::{RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use crate::value::{ReturnValue, Value};
use crate::primitive::Primitive;
use crate::scope::Scope;
use crate::tokenizer::{Lexeme, Token};

pub trait Expression {
    fn to_string(&self) -> String;
    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String>;
    #[allow(unused_variables)]
    fn resolve(&self, scope: &mut Scope) -> Result<(), String> { Ok(()) }
    fn to_variable(&self) -> Option<&VariableExpression> {
        None
    }

    #[allow(unused)]
    fn to_function_call(&self) -> Option<&FunctionCallExpression> {
        None
    }
    fn needs_subscope(&self) -> bool {
        false
    }
}

pub trait Function {
    fn to_string(&self) -> String;
    fn evaluate(&self, args: Vec<Value>) -> Result<Value, String>;
    fn resolve(&self) -> Result<(), String> { Ok(()) }
}

pub trait Method {
    #[allow(unused)]
    fn to_string(&self) -> String;
    fn evaluate(&self, this: &mut Scope, args: Vec<Value>) -> Result<Value, String>;
    #[allow(unused)]
    fn resolve(&self) -> Result<(), String> { Ok(()) }
    fn get_name(&self) -> &str;
    fn get_lexeme(&self) -> &Lexeme;
    fn get_args(&self) -> &Vec<String>;
    fn get_body(&self) -> Arc<Box<dyn Expression>>;
}

pub trait Class {
    fn to_string(&self) -> String;
    fn invoke(&self, method: &String, this: &mut Scope, args: Vec<Value>) -> Result<Value, String>;
    fn detach_method(&self, method: &String, this: Scope) -> Result<Value, String>;
}

pub fn new_instance(class: Arc<Box<dyn Class>>, _args: Vec<Value>) -> Result<Value, String> {
    let object = SimpleObject {
        class: class.clone(),
        fields: Scope::new(),
    };
    let arc = Arc::new(RefCell::new(object));
    arc.borrow_mut().fields.define("this".to_string(), Value::Object(arc.clone()));
    arc.borrow_mut().fields.define(class.to_string(), Value::Class(class.clone()));
    Ok(Value::Object(arc))
}

pub trait Object {
    fn get_class(&self) -> Arc<Box<dyn Class>>;
    fn to_string(&self) -> String {
        self.get_class().to_string()
    }
    #[allow(unused)]
    fn as_any(&self) -> &dyn Any;
    fn get_field(&self, field: &String) -> Result<Value, String>;
    fn set_field(&mut self, field: &String, value: Value) -> Result<Value, String>;
    fn call(&self, method: &String, args: Vec<Value>) -> Result<Value, String>;
}

pub struct SimpleClass {
    name: String,
    methods: HashMap<String, Box<dyn Method>>,
}

impl Class for SimpleClass {
    fn to_string(&self) -> String {
        self.name.clone()
    }

    fn invoke(&self, method: &String, this: &mut Scope, args: Vec<Value>) -> Result<Value, String> {
        match self.methods.get(method) {
            None => {
                Err(format!("Method '{}' not found", method))
            }
            Some(m) => {
                m.evaluate(this, args)
            }
        }
    }

    fn detach_method(&self, method: &String, scope: Scope) -> Result<Value, String> {
        match self.methods.get(method) {
            None => {
                Err(format!("Method '{}' not found", method))
            }
            Some(m) => {
                let fun: Arc<Box<dyn Function>> = Arc::new(Box::new(FunctionExpression {
                    lexeme: m.get_lexeme().clone(),
                    name: m.get_name().to_string(),
                    args: m.get_args().clone(),
                    body: m.get_body(),
                    scope,
                }));
                Ok(Value::Function(fun.clone()))
            }
        }
    }
}

pub struct SimpleObject{
    class: Arc<Box<dyn Class>>,
    fields: Scope,
}

impl Object for SimpleObject {
    fn get_class(&self) -> Arc<Box<dyn Class>> {
        self.class.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn to_string(&self) -> String {
        format!("{} instance", self.class.to_string())
    }

    fn get_field(&self, field: &String) -> Result<Value, String> {
        match self.fields.get(field) {
            None => {
                self.class.detach_method(field, self.fields.subscope())
            }
            Some(v) => {
                Ok(v.borrow().clone())
            }
        }
    }
    fn set_field(&mut self, field: &String, value: Value) -> Result<Value, String> {
        self.fields.define(field.clone(), value.clone());
        Ok(value)
    }
    fn call(&self, method: &String, args: Vec<Value>) -> Result<Value, String> {
        if let Some(f) = self.fields.get(method) {
            return match &(*f.borrow()) {
                Value::Function(f) => {
                    f.evaluate(args)
                },
                _ => Err(format!("Field '{}' is not a function", method))
            }
        }
        let mut scope = self.fields.subscope();
        let result = self.class.invoke(method, &mut scope, args);
        result
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
        IfExpression { condition, body, else_body }
    }
}

#[allow(unused)]
pub struct WhileExpression {
    condition: Box<dyn Expression>,
    body: Box<dyn Expression>,
}

impl WhileExpression {
    pub fn new(condition: Box<dyn Expression>, body: Box<dyn Expression>) -> Self {
        WhileExpression { condition, body }
    }
}

#[allow(unused)]
pub struct ForExpression {
    before: Option<Box<dyn Expression>>,
    condition: Box<dyn Expression>,
    after: Option<Box<dyn Expression>>,
    body: Box<dyn Expression>,
}

impl ForExpression {
    pub fn new(before: Option<Box<dyn Expression>>, condition: Box<dyn Expression>, after: Option<Box<dyn Expression>>, body: Box<dyn Expression>) -> Self {
        ForExpression { before, condition, after, body }
    }
}

#[allow(unused)]
pub struct BlockExpression {
    start: Lexeme,
    end: Lexeme,
    expressions: Vec<Box<dyn Expression>>,
    scoped: bool
}

impl BlockExpression {
    pub fn new(start: Lexeme, end: Lexeme, expressions: Vec<Box<dyn Expression>>, create_subscope_on_execution: bool) -> Self {
        let scoped = create_subscope_on_execution && expressions.iter().any(|x| x.needs_subscope());
        BlockExpression { start, end, expressions, scoped }
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
pub struct ClassDeclarationExpression {
    lexeme: Lexeme,
    name: String,
    methods: Vec<MethodDefinition>,
}

impl ClassDeclarationExpression {
    pub fn new(lexeme: Lexeme, name: String, methods: Vec<MethodDefinition>) -> Self {
        ClassDeclarationExpression { lexeme, name, methods }
    }
}

#[allow(unused)]
pub struct GetFieldExpression {
    lexeme: Lexeme,
    object: Box<dyn Expression>,
    field: String,
}

impl GetFieldExpression {
    pub fn new(lexeme: Lexeme, object: Box<dyn Expression>, field: String) -> Self {
        GetFieldExpression { lexeme, object, field }
    }
}

#[allow(unused)]
pub struct MethodCallExpression {
    lexeme: Lexeme,
    object: Box<dyn Expression>,
    method: String,
    args: Vec<Box<dyn Expression>>
}

impl MethodCallExpression {
    pub fn new(lexeme: Lexeme, object: Box<dyn Expression>, method: String, args: Vec<Box<dyn Expression>>) -> Self {
        MethodCallExpression { lexeme, object, method, args }
    }
}

#[allow(unused)]
pub struct SetFieldExpression {
    lexeme: Lexeme,
    object: Box<dyn Expression>,
    field: String,
    value: Box<dyn Expression>,
}

impl SetFieldExpression {
    pub fn new(lexeme: Lexeme, object: Box<dyn Expression>, field: String, value: Box<dyn Expression>) -> Self {
        SetFieldExpression { lexeme, object, field, value }
    }
}

#[allow(unused)]
pub struct VariableExpression {
    lexeme: Lexeme,
    name: String,
    binded: RefCell<Option<(Scope, Rc<RefCell<Value>>)>>
}

impl VariableExpression {
    pub fn new(lexeme: Lexeme, name: String) -> Self {
        VariableExpression { lexeme, name, binded: RefCell::new(None) }
    }

    #[allow(unused)]
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}


#[allow(unused)]
pub struct FunctionCallExpression {
    lexeme: Lexeme,
    name: String,
    args: Vec<Box<dyn Expression>>
}

impl FunctionCallExpression {
    pub fn new(lexeme: Lexeme, name: String, args: Vec<Box<dyn Expression>>) -> Self {
        FunctionCallExpression { lexeme, name, args }
    }
}

#[allow(unused)]
pub struct FunctionExpression {
    lexeme: Lexeme,
    name: String,
    args: Vec<String>,
    body: Arc<Box<dyn Expression>>,
    scope: Scope,
}

impl FunctionExpression {
    pub fn _new(lexeme: Lexeme, name: String, args: Vec<String>, body: Box<dyn Expression>, scope: Scope) -> Self {
        FunctionExpression { lexeme, name, args, body: Arc::new(body), scope }
    }
}

#[allow(unused)]
pub struct FunctionDefinitionExpression {
    lexeme: Lexeme,
    name: String,
    args: Vec<String>,
    body: Arc<Box<dyn Expression>>,
}

impl FunctionDefinitionExpression {
    pub fn new(lexeme: Lexeme, name: String, args: Vec<String>, body: Box<dyn Expression>) -> Self {
        FunctionDefinitionExpression { lexeme, name, args, body: Arc::new(body) }
    }
}

#[allow(unused)]
#[derive(Clone)]
pub struct MethodDefinition {
    lexeme: Lexeme,
    name: String,
    args: Vec<String>,
    body: Arc<Box<dyn Expression>>,
}

impl MethodDefinition {
    pub fn new(lexeme: Lexeme, name: String, args: Vec<String>, body: Box<dyn Expression>) -> Self {
        MethodDefinition { lexeme, name, args, body: Arc::new(body) }
    }
}

impl Method for MethodDefinition {
    fn to_string(&self) -> String {
        format!("{} ({})", self.name, self.args.join(", "))
    }

    fn evaluate(&self, this: &mut Scope, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != self.args.len() {
            return Err(format!("Method {} takes {} arguments, but got {}", self.name, self.args.len(), args.len()));
        }

        for i in (0..self.args.len()).rev() {
            this.define(self.args[i].clone(), args.remove(i));
        }

        let result = self.body.evaluate(this);
        match result {
            Ok(it) => {
                if let Value::Return(v) = it {
                    Ok(v.to_value())
                } else {
                    Ok(it)
                }
            }
            Err(e) => { Err(e) }
        }
    }
    fn get_args(&self) -> &Vec<String> {
        &self.args
    }
    fn get_body(&self) -> Arc<Box<dyn Expression>> {
        self.body.clone()
    }
    fn get_lexeme(&self) -> &Lexeme {
        &self.lexeme
    }
    fn get_name(&self) -> &str {
        self.name.as_str()
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
        match &self.literal {
            Primitive::Number(i) => {
                let number_str = i.to_string();
                if number_str.contains('.') {
                    number_str
                } else {
                    let mut x: Vec<char> = number_str.chars().collect();
                    x.push('.');
                    x.push('0');
                    x.iter().collect()
                }
            }
            p => p.to_string()
        }
        // self.literal.to_string()
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

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.expression.resolve(scope)
    }
}

impl Expression for IfExpression {
    fn to_string(&self) -> String {
        format!("(if ({}) {})", self.condition.to_string(), self.body.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let check = self.condition.evaluate(scope)?;
        if check.is_true() {
            self.body.evaluate(scope)
        } else if let Some(e) = &self.else_body {
            e.evaluate(scope)
        } else {
            Ok(Value::Primitive(Primitive::Nil))
        }
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.body.resolve(scope)
    }
}

impl Expression for WhileExpression {
    fn to_string(&self) -> String {
        format!("(while ({}) {})", self.condition.to_string(), self.body.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let mut value = Value::Primitive(Primitive::Nil);
        while self.condition.evaluate(scope)?.is_true() {
            value = self.body.evaluate(scope)?;
            if let Value::Return(v) = value {
                value = v.to_value();
                break
            }
        };
        Ok(value)
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.body.resolve(scope)
    }
}

impl Expression for ForExpression {
    fn to_string(&self) -> String {
        let before = match &self.before {
            None => { "".to_string() }
            Some(e) => { e.to_string() }
        };
        let after = match &self.after {
            None => { "".to_string() }
            Some(e) => { e.to_string() }
        };
        format!("(for ({before}; {}; {after}) {})", self.condition.to_string(), self.body.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let mut value = Value::Primitive(Primitive::Nil);
        scope.push_scope();
        if let Some(e) = &self.before {
            e.evaluate(scope)?;
        }

        while self.condition.evaluate(scope)?.is_true() {
            value = self.body.evaluate(scope)?;
            if let Value::Return(v) = value {
                value = v.to_value();
                break
            }
            if let Some(e) = &self.after {
                e.evaluate(scope)?;
            }
        };
        scope.pop_scope();
        Ok(value)
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        scope.push_scope();
        match &self.before {
            None => {}
            Some(e) => { e.resolve(scope)?; }
        };
        match &self.after {
            None => {}
            Some(e) => { e.resolve(scope)?; }
        };
        self.condition.resolve(scope)?;
        self.body.resolve(scope)?;
        scope.pop_scope();
        Ok(())
    }
}

impl Expression for BlockExpression {
    fn to_string(&self) -> String {
        let expressions: Vec<String> = self.expressions.iter().map(|x| x.to_string()).collect();
        format!("(block {})", expressions.join(", "))
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let mut value = Value::Primitive(Primitive::Nil);

        if self.scoped {
            scope.push_scope();
        }
        for x in &self.expressions {
            value = x.evaluate(scope)?;
            if let Value::Return(_) = &value {
                break
            }
        }
        if self.scoped {
            scope.pop_scope();
        }
        Ok(value)
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        if self.scoped {
            scope.push_scope();
        }
        for x in &self.expressions {
            x.resolve(scope)?;
        }
        if self.scoped {
            scope.pop_scope();
        }
        Ok(())
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
            Value::Object(e) => { Err(format!("Cannot apply unary not to an object {}", e.borrow().to_string())) }
            Value::Class(e) => { Err(format!("Cannot apply unary not to a function {}", e.to_string())) }
            Value::Function(e) => { Err(format!("Cannot apply unary not to a function {}", e.to_string())) }
            Value::Return(e) => { Err(format!("Cannot apply unary not to a return {}", e.to_string())) }
            Value::Uninitialized => Err("Cannot apply unary not to uninitialized value".to_string()),
        }
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.expression.resolve(scope)
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
            Value::Object(e) => { Err(format!("Cannot apply unary minus to an object {}", e.borrow().to_string())) }
            Value::Class(e) => { Err(format!("Cannot apply unary minus to a function {}", e.to_string())) }
            Value::Function(e) => { Err(format!("Cannot apply unary minus to a function {}", e.to_string())) }
            Value::Return(e) => { Err(format!("Cannot apply unary minus to a return {}", e.to_string())) }
            Value::Uninitialized => Err("Cannot apply unary minus to uninitialized value".to_string()),
        }
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.expression.resolve(scope)
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
            Value::Object(e) => { println!("{}", e.borrow().to_string()) }
            Value::Class(e) => { println!("{}", e.to_string()) }
            Value::Function(e) => { println!("{}", e.to_string()) }
            Value::Return(it) => { println!("return {}", it.to_string()) }
            Value::Uninitialized => return Err("cannot print uninitialized value".to_string()),
        }
        Ok(Value::Primitive(Primitive::Nil))
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.expression.resolve(scope)
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

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        if !scope.is_global() {
            if scope.is_defined_in_this_scope(&self.name) {
                return Err(format!("[line {}] Variable {} already defined", self.lexeme.line, self.name));
            }
            scope.define(self.name.clone(), Value::Uninitialized);
        }

        self.expression.resolve(scope)?;
        scope.define(self.name.clone(), Value::Primitive(Primitive::Nil));
        Ok(())
    }

    fn needs_subscope(&self) -> bool {
        true
    }
}

impl Expression for ClassDeclarationExpression {
    fn to_string(&self) -> String {
        format!("class {}", self.name.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let mut methods: HashMap<String, Box<dyn Method>> = HashMap::new();
        self.methods.iter().for_each(|m| { methods.insert(m.name.clone(), Box::new(m.clone())); });
        let class: Arc<Box<dyn Class>> = Arc::new(Box::new(SimpleClass { name: self.name.clone(), methods }));
        // let class: Arc<RefCell<dyn Object>> = Arc::new(RefCell::new(ClassObject { class }));

        scope.define(self.name.clone(), Value::Class(class.clone()));

        Ok(Value::Class(class))
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        let _ = self.evaluate(scope)?;
        Ok(())
    }
    fn needs_subscope(&self) -> bool {
        true
    }
}

impl Expression for GetFieldExpression {
    fn to_string(&self) -> String {
        format!("{}.{}", self.object.to_string(), self.field)
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let object = self.object.evaluate(scope)?;
        match object {
            Value::Object(o) => {
                o.borrow().get_field(&self.field)
            }
            v => {
                Err(format!("Cannot get field {} from not an object {} at line {}", self.field, v.to_string(), self.lexeme.line))
            }
        }
    }

    fn resolve(&self, _scope: &mut Scope) -> Result<(), String> {
        // let _ = self.evaluate(scope)?;
        Ok(())
    }
}

impl Expression for MethodCallExpression {
    fn to_string(&self) -> String {
        format!("{}.{}()", self.object.to_string(), self.method)
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let object = self.object.evaluate(scope)?;
        match object {
            Value::Object(o) => {
                let mut args = Vec::with_capacity(self.args.len());
                for a in &self.args {
                    args.push(a.evaluate(scope)?);
                }
                o.borrow().call(&self.method, args)
            }
            v => {
                Err(format!("Cannot call method {} from not an object {} at line {}", self.method, v.to_string(), self.lexeme.line))
            }
        }
    }

    fn resolve(&self, _scope: &mut Scope) -> Result<(), String> {
        // let _ = self.evaluate(scope)?;
        Ok(())
    }
}

impl Expression for SetFieldExpression {
    fn to_string(&self) -> String {
        format!("{}.{}", self.object.to_string(), self.field)
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let object = self.object.evaluate(scope)?;
        match object {
            Value::Object(o) => {
                let value = self.value.evaluate(scope)?;
                o.borrow_mut().set_field(&self.field, value)
            }
            v => {
                Err(format!("Cannot set field {} of not an object {} at line {}", self.object.to_string(), v.to_string(), self.lexeme.line))
            }
        }
    }

    fn resolve(&self, _scope: &mut Scope) -> Result<(), String> {
        // let _ = self.evaluate(scope)?;
        Ok(())
    }
}

impl Expression for VariableExpression {
    fn to_string(&self) -> String {
        format!("var {}", self.name)
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        match self.get(scope) {
            None => {
                Err(format!("Variable {} not found", self.name))
            }
            Some(v) => {
                Ok(v)
            }
        }
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        match self.get(scope) {
            None => {
                Err(format!("Variable {} not found", self.name))
            }
            Some(v) => {
                if let Value::Uninitialized = v {
                    return Err(format!("Variable {} is uninitialized", self.name));
                }
                Ok(())
            }
        }
    }

    fn to_variable(&self) -> Option<&Self> {
        Some(self)
    }
}

impl VariableExpression {
    fn set(&self, scope: &mut Scope, value: Value) {
        if let Some((binded_scope, value_holder)) = self.binded.borrow().as_ref() {
            if binded_scope.equals(scope) {
                value_holder.replace(value.clone());
                return;
            }
        }

        if let Some(v) = scope.get(&self.name) {
            self.binded.replace(Some((scope.clone(), v.clone())));
        }

        scope.set(&self.name, value.clone());
    }

    fn get(&self, scope: &Scope) -> Option<Value> {
        if let Some((binded_scope, value_holder)) = self.binded.borrow().as_ref() {
            if binded_scope.equals(scope) {
                return Some(value_holder.borrow().clone());
            }
        }

        match scope.get(&self.name) {
            None => {
                None
            }
            Some(v) => {
                self.binded.replace(Some((scope.clone(), v.clone())));
                Some(v.borrow().clone())
            }
        }
    }
}

impl Expression for FunctionCallExpression {
    fn to_string(&self) -> String {
        format!("{}()", self.name)
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        match scope.get(&self.name) {
            None => {
                Err(format!("Function {} not found", self.name))
            }
            Some(v) => {
                match &(*v.borrow()) {
                    Value::Function(e) => {
                        let mut args = Vec::with_capacity(self.args.len());
                        for a in &self.args {
                            args.push(a.evaluate(scope)?);
                        }

                        e.evaluate(args)
                    }
                    Value::Class(e) => {
                        let mut args = Vec::with_capacity(self.args.len());
                        for a in &self.args {
                            args.push(a.evaluate(scope)?);
                        }

                        new_instance(e.clone(), args)
                    }
                    _ => {
                        Err(format!("variable {} is not a function", self.name))
                    }
                }
            }
        }
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        for a in &self.args {
            a.resolve(scope)?
        }
        Ok(())
    }
    fn to_function_call(&self) -> Option<&FunctionCallExpression> {
        Some(self)
    }
}

impl Expression for FunctionDefinitionExpression {
    fn to_string(&self) -> String {
        format!("fun {}()", self.name)
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let mut function_scope = scope.clone_scope();
        let fun: Arc<Box<dyn Function>> = Arc::new(Box::new(FunctionExpression {
            lexeme: self.lexeme.clone(),
            name: self.name.clone(),
            args: self.args.clone(),
            body: self.body.clone(),
            scope: function_scope.clone(),
        }));
        scope.define(self.name.clone(), Value::Function(fun.clone()));
        function_scope.define(self.name.clone(), Value::Function(fun.clone()));
        Ok(Value::Function(fun.clone()))
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        for arg in &self.args {
            if 1 != self.args.iter().filter(|x| x.eq(&arg)).count() {
                return Err(format!("Argument {} declared more than once", arg));
            }
        }

        scope.define("%in function%".to_string(), Value::Primitive(Primitive::Nil));
        let f = self.evaluate(scope)?;
        if let Value::Function(f) = f {
            f.resolve()?;
        }
        scope.remove(&"%in function%".to_string());
        Ok(())
    }

    fn needs_subscope(&self) -> bool {
        true
    }
}

impl Function for FunctionExpression {
    fn to_string(&self) -> String {
        format!("<fn {}>", self.name)
    }

    fn evaluate(&self, mut args: Vec<Value>) -> Result<Value, String> {
        if args.len() != self.args.len() {
            return Err(format!("Function {} takes {} arguments, but got {}", self.name, self.args.len(), args.len()));
        }

        let mut scope = self.scope.clone();
        scope.push_scope();

        for i in (0..self.args.len()).rev() {
            scope.define(self.args[i].clone(), args.remove(i));
        }

        let result = self.body.evaluate(&mut scope);
        scope.pop_scope();
        match result {
            Ok(it) => {
                if let Value::Return(v) = it {
                    Ok(v.to_value())
                } else {
                    Ok(it)
                }
            }
            Err(e) => { Err(e) }
        }
    }

    fn resolve(&self) -> Result<(), String> {
        let mut scope = self.scope.clone();
        scope.push_scope();

        for i in (0..self.args.len()).rev() {
            scope.define(self.args[i].clone(), Value::Primitive(Primitive::Nil));
        }
        self.body.resolve(&mut scope)?;
        scope.pop_scope();
        Ok(())
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
            Token::OR => { "or" }
            Token::AND => { "and" }
            t => { panic!("{:?} is not an action for binary expression", t) }
        };
        format!("({} {} {})", action, self.left.to_string(), self.right.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        if self.lexeme.token == Token::EQUAL {
            return match self.left.to_variable() {
                Some(variable) => {
                    let value = self.right.evaluate(scope)?;
                    variable.set(scope, value.clone());
                    Ok(value)
                }
                None => {
                    Err(format!("Cannot assign not a variable: {}", self.left.to_string()))
                    // std::process::exit(65);
                }
            };
        }

        if self.lexeme.token == Token::AND {
            let left = self.left.evaluate(scope)?;
            if left.is_true() {
                let right = self.right.evaluate(scope)?;
                if right.is_true() {
                    return Ok(right);
                } else {
                    return Ok(Value::Primitive(Primitive::Boolean(false)));
                }
            } else {
                return Ok(Value::Primitive(Primitive::Boolean(false)));
            }
        }

        let left = self.left.evaluate(scope)?;
        if self.lexeme.token == Token::OR {
            if left.is_true() {
                return Ok(left);
            }
        }

        let right = self.right.evaluate(scope)?;
        if self.lexeme.token == Token::OR {
            return Ok(right);
        }

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
            (a, b) => {
                Err(format!("Cannot apply {:?} expression to {} and {}", self.lexeme.token, a.to_string(), b.to_string()))
            }
        }
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.left.resolve(scope)?;
        self.right.resolve(scope)
    }
}


#[allow(unused)]
pub struct NativeFunctionExpression {
    name: String,
    fun: Box<dyn Fn(Vec<Value>) -> Result<Value, String>>,
}

impl NativeFunctionExpression {
    pub fn new(name: String, fun: Box<dyn Fn(Vec<Value>) -> Result<Value, String>>) -> Self {
        NativeFunctionExpression { name, fun }
    }
}
impl Function for NativeFunctionExpression {
    fn to_string(&self) -> String {
        self.name.clone()
    }

    fn evaluate(&self, args: Vec<Value>) -> Result<Value, String> {
        (self.fun)(args)
    }
}



#[allow(unused)]
pub struct ReturnExpression {
    lexeme: Lexeme,
    expression: Box<dyn Expression>,
}

impl ReturnExpression {
    pub fn new(lexeme: Lexeme, expression: Box<dyn Expression>) -> Self {
        ReturnExpression { lexeme, expression }
    }
}

impl Expression for ReturnExpression {
    fn to_string(&self) -> String {
        format!("return {}", self.expression.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let value = self.expression.evaluate(scope)?;
        Ok(Value::Return(ReturnValue::from_value(value)))
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        let key = "%in function%".to_string();
        match scope.get(&key) {
            None => Err(format!("Cannot return from top level")),
            Some(_) => Ok(())
        }
    }
}


#[allow(unused)]
pub struct AnonymousFunctionCallExpression {
    lexeme: Lexeme,
    fun: Box<dyn Expression>,
    args: Vec<Box<dyn Expression>>
}

impl AnonymousFunctionCallExpression {
    pub fn new(lexeme: Lexeme, fun: Box<dyn Expression>, args: Vec<Box<dyn Expression>>) -> Self {
        AnonymousFunctionCallExpression { lexeme, fun, args }
    }
}

impl Expression for AnonymousFunctionCallExpression {
    fn to_string(&self) -> String {
        format!("{}()", self.fun.to_string())
    }

    fn evaluate(&self, scope: &mut Scope) -> Result<Value, String> {
        let fun = match self.fun.evaluate(scope)? {
            Value::Function(e) => {
                Ok(e.clone())
            }
            _ => {
                Err(format!("expression {} is not a function", self.fun.to_string()))
            }
        }?;
        let mut args = Vec::with_capacity(self.args.len());
        for a in &self.args {
            args.push(a.evaluate(scope)?);
        }
        fun.evaluate(args)
    }

    fn resolve(&self, scope: &mut Scope) -> Result<(), String> {
        self.fun.resolve(scope)
    }
}