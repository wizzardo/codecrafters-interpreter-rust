use crate::expression::{Function};
use crate::primitive::Primitive;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

#[derive(Clone)]
pub enum Value {
    Primitive(Primitive),
    Function(Arc<Box<dyn Function>>),
}

#[allow(unused)]
impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Primitive(p) => p.to_string(),
            Value::Function(e) => e.to_string(),
        }
    }
    pub fn is_true(&self) -> bool {
        match self {
            Value::Primitive(p) => match p {
                Primitive::Number(v) => *v != 0.0,
                Primitive::String(v) => true,
                Primitive::Boolean(v) => *v,
                Primitive::Nil => false,
            },
            Value::Function(_) => true,
        }
    }

    pub fn from_function(e: Box<dyn Function>) -> Self {
        Value::Function(Arc::new(e))
    }
    pub fn from_number(it: f64) -> Self {
        Value::Primitive(Primitive::Number(it))
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
