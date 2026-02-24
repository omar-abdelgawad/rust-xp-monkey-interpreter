use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum SymbolScope {
    GLOBAL,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    pub index: usize,
}
impl Symbol {
    fn new(name: impl Into<String>, scope: SymbolScope, index: usize) -> Self {
        Self {
            name: name.into(),
            scope,
            index,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
    pub fn define(&mut self, ident: String) -> Symbol {
        let symbol = Symbol::new(ident.clone(), SymbolScope::GLOBAL, self.num_definitions);
        self.store.insert(ident, symbol.clone());
        self.num_definitions += 1;
        symbol
    }
    pub fn resolve(&self, ident: &str) -> Option<Symbol> {
        self.store.get(ident).cloned()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_define() {
        let expected = HashMap::from([
            ("a", Symbol::new("a", SymbolScope::GLOBAL, 0)),
            ("b", Symbol::new("b", SymbolScope::GLOBAL, 1)),
        ]);
        let mut global = SymbolTable::new();
        let a = global.define("a".to_string());
        assert_eq!(
            a, expected["a"],
            "expected a={:?}, got={:?}",
            expected["a"], a
        );
        let b = global.define("b".to_string());
        assert_eq!(
            b, expected["b"],
            "expected b={:?}, got={:?}",
            expected["b"], b
        );
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let expected = vec![
            Symbol::new("a", SymbolScope::GLOBAL, 0),
            Symbol::new("b", SymbolScope::GLOBAL, 1),
        ];
        for sym in expected {
            let res = global
                .resolve(&sym.name)
                .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));
            assert_eq!(
                res, sym,
                "expected {} to resolve to {:?}, got={:?}",
                sym.name, sym, res
            );
        }
    }
}
