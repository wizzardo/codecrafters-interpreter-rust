use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;
use crate::value::Value;

// #[derive(Debug)]
// pub struct Scope {
//     stack: Vec<HashMap<String, Value>>,
// }
// 
// impl Scope {
//     pub fn new() -> Self {
//         let mut stack: Vec<HashMap<String, Value>> = vec![];
//         stack.push(HashMap::new());
//         Scope { stack }
//     }
//     pub fn push_scope(&mut self) {
//         self.stack.push(HashMap::new());
//     }
//     pub fn pop_scope(&mut self) {
//         let _ = self.stack.pop();
//     }
// 
//     pub fn define(&mut self, key: String, value: Value) {
//         self.stack.last_mut().expect("Scope stack is empty").insert(key, value);
//     }
//     pub fn set(&mut self, key: String, value: Value) {
//         for i in (0..self.stack.len()).rev() {
//             let map = self.stack.get_mut(i).unwrap();
//             match map.entry(key.clone()) {
//                 Entry::Occupied(mut e) => {
//                     e.insert(value);
//                     break;
//                 }
//                 Entry::Vacant(_) => {
//                     continue;
//                 }
//             };
//         }
//     }
//     pub fn get(&self, key: &String) -> Option<&Value> {
//         for i in (0..self.stack.len()).rev() {
//             let option = self.stack.get(i).unwrap().get(key);
//             if option.is_some() {
//                 return option;
//             }
//         }
//         None
//     }
// }

#[derive(Debug, Clone)]
pub struct Scope {
    current: Rc<ScopeNode>,
}

#[derive(Debug, Clone)]
struct ScopeNode {
    parent: Option<Rc<ScopeNode>>,
    data: RefCell<HashMap<String, Rc<RefCell<Value>>>>,
}

impl Scope {
    pub fn new() -> Self {
        let node = Rc::new(ScopeNode { parent: None, data: RefCell::new(HashMap::new()) });
        Scope { current: node }
    }
    pub fn push_scope(&mut self) {
        let parent: Option<Rc<ScopeNode>> = Some(self.current.clone());
        let next = Rc::new(ScopeNode { parent, data: RefCell::new(HashMap::new()) });
        self.current = next;
    }
    pub fn pop_scope(&mut self) {
        self.current = self.current.parent.as_ref().unwrap().clone();
    }
    pub fn define(&mut self, key: String, value: Value) {
        self.current.data.borrow_mut().insert(key, Rc::new(RefCell::new(value)));
    }
    pub fn set(&mut self, key: String, value: Value) {
        let mut node = self.current.as_ref();
        loop {
            let mut map = node.data.borrow_mut();
            match map.entry(key.clone()) {
                Entry::Occupied(mut e) => {
                    e.insert(Rc::new(RefCell::new(value)));
                    break;
                }
                Entry::Vacant(_) => {
                    if let Some(parent) = &node.parent {
                        node = parent
                    } else {
                        panic!("Scope not found");
                    }
                }
            };
        }
    }
    pub fn get(&self, key: &String) -> Option<Rc<RefCell<Value>>> {
        let mut node = self.current.as_ref();
        loop {
            let map = node.data.borrow();
            let option = map.get(key);
            if let Some(v) = option {
                return Some(v.clone());
            }
            if let Some(parent) = &node.parent {
                node = parent
            } else {
                break
            }
        }
        None
    }
}