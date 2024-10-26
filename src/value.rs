use crate::primitive::Primitive;

#[derive(Clone)]
#[derive(Debug)]
pub enum Value {
    Primitive(Primitive),
}

#[allow(unused)]
impl Value {
    pub fn to_string(&self) -> String {
        match self { Value::Primitive(p) => { p.to_string() } }
    }
    pub fn is_true(&self) -> bool {
        match self {
            Value::Primitive(p) => {
                match p {
                    Primitive::Number(v) => { *v != 0.0 }
                    Primitive::String(v) => { true }
                    Primitive::Boolean(v) => { *v }
                    Primitive::Nil => { false }
                }
            }
        }
    }
}