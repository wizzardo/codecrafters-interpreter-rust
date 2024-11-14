#[derive(Clone)]
#[derive(Debug)]
pub enum Primitive {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Primitive {
    pub fn to_string(&self) -> String {
        match &self {
            Primitive::Number(value) => { value.to_string() }
            Primitive::String(it) => { it.clone() }
            Primitive::Boolean(it) => { it.to_string() }
            Primitive::Nil => { "nil".to_string() }
        }
    }
}