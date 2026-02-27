use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
    Local,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    name: String,
    pub scope: SymbolScope,
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
pub type SymbolTableRef = Rc<RefCell<SymbolTable>>;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Rc<RefCell<SymbolTable>>>,

    store: HashMap<String, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
    pub fn new_enclosed_symbol_table(outer: SymbolTableRef) -> SymbolTableRef {
        let mut s = SymbolTable::new();
        s.outer = Some(outer);
        Rc::new(RefCell::new(s))
    }
    pub fn define(&mut self, ident: String) -> Symbol {
        let mut symbol = Symbol::new(ident.clone(), SymbolScope::Global, self.num_definitions);
        match self.outer {
            None => symbol.scope = SymbolScope::Global,
            Some(_) => symbol.scope = SymbolScope::Local,
        }
        self.store.insert(ident, symbol.clone());
        self.num_definitions += 1;
        symbol
    }
    pub fn resolve(&self, ident: &str) -> Option<Symbol> {
        let obj = self.store.get(ident);
        match obj {
            None => match &self.outer {
                Some(outer_symb_table) => outer_symb_table.borrow().resolve(ident),
                None => None,
            },
            Some(symb) => Some(symb.clone()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_define() {
        let expected = HashMap::from([
            ("a", Symbol::new("a", SymbolScope::Global, 0)),
            ("b", Symbol::new("b", SymbolScope::Global, 1)),
            ("c", Symbol::new("c", SymbolScope::Local, 0)),
            ("d", Symbol::new("d", SymbolScope::Local, 1)),
            ("e", Symbol::new("e", SymbolScope::Local, 0)),
            ("f", Symbol::new("f", SymbolScope::Local, 1)),
        ]);
        let global = Rc::new(RefCell::new(SymbolTable::new()));
        let a = global.borrow_mut().define("a".to_string());
        assert_eq!(
            a, expected["a"],
            "expected a={:?}, got={:?}",
            expected["a"], a
        );
        let b = global.borrow_mut().define("b".to_string());
        assert_eq!(
            b, expected["b"],
            "expected b={:?}, got={:?}",
            expected["b"], b
        );

        let mut first_local = SymbolTable::new_enclosed_symbol_table(Rc::clone(&global));
        let c = first_local.borrow_mut().define("c".to_string());
        assert_eq!(
            c, expected["c"],
            "expected c={:?}, got={:?}",
            expected["c"], c
        );
        let d = first_local.borrow_mut().define("d".to_string());
        assert_eq!(
            d, expected["d"],
            "expected d={:?}, got={:?}",
            expected["d"], d
        );

        let mut second_local = SymbolTable::new_enclosed_symbol_table(Rc::clone(&first_local));
        let e = second_local.borrow_mut().define("e".to_string());
        assert_eq!(
            e, expected["e"],
            "expected e={:?}, got={:?}",
            expected["e"], e
        );
        let f = second_local.borrow_mut().define("f".to_string());
        assert_eq!(
            f, expected["f"],
            "expected f={:?}, got={:?}",
            expected["f"], f
        );
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let expected = vec![
            Symbol::new("a", SymbolScope::Global, 0),
            Symbol::new("b", SymbolScope::Global, 1),
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
    #[test]
    fn test_resolve_local() {
        let mut global = Rc::new(RefCell::new(SymbolTable::new()));
        global.borrow_mut().define("a".to_string());
        global.borrow_mut().define("b".to_string());

        let mut local = SymbolTable::new_enclosed_symbol_table(Rc::clone(&global));
        local.borrow_mut().define("c".to_string());
        local.borrow_mut().define("d".to_string());

        let expected = vec![
            Symbol::new("a", SymbolScope::Global, 0),
            Symbol::new("b", SymbolScope::Global, 1),
            Symbol::new("c", SymbolScope::Local, 0),
            Symbol::new("d", SymbolScope::Local, 1),
        ];
        for sym in expected {
            let res = local
                .borrow()
                .resolve(&sym.name)
                .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));
            assert_eq!(
                res, sym,
                "expected {} to resolve to {:?}, got={:?}",
                sym.name, sym, res
            );
        }
    }
    #[test]
    fn test_resolve_nested_local() {
        let global = Rc::new(RefCell::new(SymbolTable::new()));
        global.borrow_mut().define("a".to_string());
        global.borrow_mut().define("b".to_string());

        let first_local = SymbolTable::new_enclosed_symbol_table(global.clone());
        first_local.borrow_mut().define("c".to_string());
        first_local.borrow_mut().define("d".to_string());

        let mut second_local = SymbolTable::new_enclosed_symbol_table(first_local.clone());
        second_local.borrow_mut().define("e".to_string());
        second_local.borrow_mut().define("f".to_string());

        let tests = vec![
            (
                first_local,
                vec![
                    Symbol::new("a", SymbolScope::Global, 0),
                    Symbol::new("b", SymbolScope::Global, 1),
                    Symbol::new("c", SymbolScope::Local, 0),
                    Symbol::new("d", SymbolScope::Local, 1),
                ],
            ),
            (
                second_local,
                vec![
                    Symbol::new("a", SymbolScope::Global, 0),
                    Symbol::new("b", SymbolScope::Global, 1),
                    Symbol::new("e", SymbolScope::Local, 0),
                    Symbol::new("f", SymbolScope::Local, 1),
                ],
            ),
        ];
        for (table, expected_symbols) in tests {
            for sym in expected_symbols {
                let res = table
                    .borrow()
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
}
