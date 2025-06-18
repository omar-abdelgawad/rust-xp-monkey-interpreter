use std::collections::HashMap;

use super::{Object, NULL};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        let s = HashMap::new();
        Self {
            store: s,
            outer: None,
        }
    }
    pub fn get(&self, name: &str) -> Option<Object> {
        // TODO: cloned should be removed
        // and eval() should return Option<&Object>
        // since the actual values are owned by the env
        let mut obj = self.store.get(name).cloned();
        if obj.is_none() {
            if let Some(outer_env) = &self.outer {
                obj = outer_env.borrow().get(name);
            }
        }
        obj
    }
    /// returns the old value for the indent or NULL
    pub fn set(&mut self, name: String, val: Object) -> Object {
        let old_val = self.store.insert(name, val);
        match old_val {
            Some(obj) => obj,
            None => NULL,
        }
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
}
