use crate::object::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Environment) -> Environment {
        let mut env = Environment::new();
        env.outer = Some(Box::new(outer));
        return env;
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(s) => Some(s.clone()),
            None => {
                let result = match &self.outer {
                    None => None,
                    Some(e) => match &e.get(name) {
                        Some(s) => Some(s.clone()),
                        None => None,
                    },
                };
                if let Some(_) = result {
                    return result;
                } else {
                    return None;
                }
            }
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }
}
