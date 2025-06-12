use std::collections::HashMap;

use super::Object;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        let s = HashMap::new();
        Self { store: s }
    }
    pub fn get(&self, name: &str) -> Option<Object> {
        // TODO: cloned should be removed
        // and eval should return Option<&Object>
        // since the actual values are owned by the env
        self.store.get(name).cloned()
    }
    /// returns the old value for the indent
    pub fn set(&mut self, name: String, val: Object) -> Option<Object> {
        self.store.insert(name, val)
    }
}
