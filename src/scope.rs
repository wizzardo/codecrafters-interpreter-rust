use std::collections::HashMap;
use std::collections::hash_map::Entry;
use crate::value::Value;

#[derive(Debug)]
pub struct Scope {
    stack: Vec<HashMap<String, Value>>,
}

impl Scope {
    pub fn new() -> Self {
        let mut stack: Vec<HashMap<String, Value>> = vec![];
        stack.push(HashMap::new());
        Scope { stack }
    }
    pub fn push_scope(&mut self) {
        self.stack.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        let _ = self.stack.pop();
    }

    pub fn define(&mut self, key: String, value: Value) {
        self.stack.last_mut().expect("Scope stack is empty").insert(key, value);
    }
    pub fn set(&mut self, key: String, value: Value) {
        for i in (0..self.stack.len()).rev() {
            let map = self.stack.get_mut(i).unwrap();
            match map.entry(key.clone()) {
                Entry::Occupied(mut e) => {
                    e.insert(value);
                    break;
                }
                Entry::Vacant(_) => {
                    continue;
                }
            };
        }
    }
    pub fn get(&self, key: &String) -> Option<&Value> {
        for i in (0..self.stack.len()).rev() {
            let option = self.stack.get(i).unwrap().get(key);
            if option.is_some() {
                return option;
            }
        }
        None
    }
}