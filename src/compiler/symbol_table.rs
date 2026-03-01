use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function, // only define one symbol with this symbscope per scope for self-referencing
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
    pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: vec![],
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
    pub fn define_builtin(&mut self, index: usize, name: String) -> Symbol {
        let symbol = Symbol::new(name.clone(), SymbolScope::Builtin, index);
        self.store.insert(name, symbol.clone());
        symbol
    }
    pub fn define_free(&mut self, original: Symbol) -> Symbol {
        self.free_symbols.push(original.clone());

        let symbol = Symbol::new(
            original.name.clone(),
            SymbolScope::Free,
            self.free_symbols.len() - 1,
        );

        self.store.insert(original.name, symbol.clone());
        symbol
    }
    pub fn define_function_name(&mut self, name: String) -> Symbol {
        // this symbols index choice is kind of arbitrary and doesn't matter
        let symbol = Symbol::new(name.clone(), SymbolScope::Function, 0);
        self.store.insert(name, symbol.clone());
        symbol
    }
    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        // AI generated logic cause I am in a hurry and lazy
        // 1️⃣ Check local scope
        if let Some(sym) = self.store.get(name) {
            return Some(sym.clone());
        }

        // 2️⃣ Check outer scope
        let outer = self.outer.as_ref()?; // no outer scope means global scope
        let resolved = outer.borrow_mut().resolve(name)?;

        // 3️⃣ If global or builtin → return as-is
        match resolved.scope {
            SymbolScope::Global | SymbolScope::Builtin => Some(resolved),

            // 4️⃣ Otherwise it's a free symbol
            _ => {
                let free = self.define_free(resolved);
                Some(free)
            }
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
                .borrow_mut()
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
                    .borrow_mut()
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
    #[test]
    fn test_define_resolve_builtins() {
        let mut global = Rc::new(RefCell::new(SymbolTable::new()));
        let mut first_local = SymbolTable::new_enclosed_symbol_table(Rc::clone(&global));
        let mut second_local = SymbolTable::new_enclosed_symbol_table(Rc::clone(&first_local));

        let expected = vec![
            Symbol::new("a", SymbolScope::Builtin, 0),
            Symbol::new("c", SymbolScope::Builtin, 1),
            Symbol::new("e", SymbolScope::Builtin, 2),
            Symbol::new("f", SymbolScope::Builtin, 3),
        ];
        for (i, v) in expected.iter().enumerate() {
            global.borrow_mut().define_builtin(i, v.name.clone());
        }
        for table in [global, first_local, second_local] {
            for sym in expected.iter() {
                let result = table
                    .borrow_mut()
                    .resolve(&sym.name)
                    .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));
                assert_eq!(
                    &result, sym,
                    "expected {} to resolve to {sym:?}, got={result:?}",
                    sym.name
                )
            }
        }
    }
    #[test]
    fn test_resolve_free() {
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
                vec![],
            ),
            (
                second_local,
                vec![
                    Symbol::new("a", SymbolScope::Global, 0),
                    Symbol::new("b", SymbolScope::Global, 1),
                    Symbol::new("c", SymbolScope::Free, 0),
                    Symbol::new("d", SymbolScope::Free, 1),
                    Symbol::new("e", SymbolScope::Local, 0),
                    Symbol::new("f", SymbolScope::Local, 1),
                ],
                vec![
                    Symbol::new("c", SymbolScope::Local, 0),
                    Symbol::new("d", SymbolScope::Local, 1),
                ],
            ),
        ];
        for (table, expected_symbols, expected_free_symbols) in tests {
            for sym in expected_symbols {
                let res = table
                    .borrow_mut()
                    .resolve(&sym.name)
                    .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));
                assert_eq!(
                    res, sym,
                    "expected {} to resolve to {sym:?}, got={res:?}",
                    sym.name,
                );
            }
            assert_eq!(
                table.borrow().free_symbols.len(),
                expected_free_symbols.len(),
                "wrong number of free symbols. got={}, want={}",
                table.borrow().free_symbols.len(),
                expected_free_symbols.len()
            );
            let table_ref = table.borrow();
            for (i, sym) in expected_free_symbols.iter().enumerate() {
                let res = table_ref
                    .free_symbols
                    .get(i)
                    .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));
                assert_eq!(res, sym, "wrong free symbol. got={res:?}, want={sym:?}",);
            }
        }
    }
    #[test]
    fn test_resolve_unresolvable_free() {
        let global = Rc::new(RefCell::new(SymbolTable::new()));
        global.borrow_mut().define("a".to_string());

        let first_local = SymbolTable::new_enclosed_symbol_table(global.clone());
        first_local.borrow_mut().define("c".to_string());

        let mut second_local = SymbolTable::new_enclosed_symbol_table(first_local.clone());
        second_local.borrow_mut().define("e".to_string());
        second_local.borrow_mut().define("f".to_string());

        let expected = vec![
            Symbol::new("a", SymbolScope::Global, 0),
            Symbol::new("c", SymbolScope::Free, 0),
            Symbol::new("e", SymbolScope::Local, 0),
            Symbol::new("f", SymbolScope::Local, 1),
        ];
        for sym in expected {
            let res = second_local
                .borrow_mut()
                .resolve(&sym.name)
                .unwrap_or_else(|| panic!("name {} not resolvable", sym.name));
            assert_eq!(
                res, sym,
                "expected {} to resolve to {sym:?}, got={res:?}",
                sym.name,
            );
        }
        let expected_unresolvable = vec!["b", "d"];
        for name in expected_unresolvable {
            let err = second_local.borrow_mut().resolve(name);
            assert!(
                err.is_none(),
                "name {name} resolved, but was expected not to"
            );
        }
    }
    #[test]
    fn test_define_and_resolve_function_name() {
        let global = Rc::new(RefCell::new(SymbolTable::new()));
        global.borrow_mut().define_function_name("a".to_string());

        let expected = Symbol::new("a", SymbolScope::Function, 0);

        let result = global
            .borrow_mut()
            .resolve(&expected.name)
            .unwrap_or_else(|| panic!("function name {} not resolvable", expected.name));
        assert_eq!(
            result, expected,
            "expected {} to resolve to {expected:?}, got={result:?}",
            expected.name,
        );
    }
    #[test]
    fn test_shadowing_function_name() {
        let global = Rc::new(RefCell::new(SymbolTable::new()));
        global.borrow_mut().define_function_name("a".to_string());
        global.borrow_mut().define("a".to_string());

        let expected = Symbol::new("a", SymbolScope::Global, 0);

        let result = global
            .borrow_mut()
            .resolve(&expected.name)
            .unwrap_or_else(|| panic!("function name {} not resolvable", expected.name));
        assert_eq!(
            result, expected,
            "expected {} to resolve to {expected:?}, got={result:?}",
            expected.name,
        );
    }
}
