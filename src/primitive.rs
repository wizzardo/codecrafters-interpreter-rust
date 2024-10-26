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