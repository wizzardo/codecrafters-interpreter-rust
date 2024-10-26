use crate::primitive::Primitive;

#[derive(Clone)]
pub enum Value {
    Primitive(Primitive),
}

#[allow(unused)]
impl Value {
    pub fn to_string(&self) -> String {
        match self { Value::Primitive(p) => { p.to_string() } }
    }
}