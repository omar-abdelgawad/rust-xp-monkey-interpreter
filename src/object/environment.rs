use std::collections::HashMap;

use super::{null_obj, ObjRef};
use std::cell::RefCell;
use std::rc::Rc;

// WARNING: never try to print struct as it can loop infinitely
#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, ObjRef>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        let s = HashMap::new();
        Self {
            store: s,
            outer: None,
        }
    }
    pub fn get(&self, name: &str) -> Option<ObjRef> {
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
    /// Always returns NULL
    pub fn set(&mut self, name: String, val: ObjRef) -> ObjRef {
        let _old_val = self.store.insert(name, val);
        null_obj()
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
}
