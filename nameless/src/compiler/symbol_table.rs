use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Scope {
    Global,
    Local,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: Scope,
    pub index: u32,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> SymbolTable {
        SymbolTable {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.into(),
            index: if let Some(symbol) = self.resolve(name) {
                symbol.index
            } else {
                self.store.len() as u32
            },
            scope: if let Some(_) = self.outer {
                Scope::Local
            } else {
                Scope::Global
            },
        };

        self.store.insert(name.into(), symbol.clone());

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        if let Some(symbol) = self.store.get(name.into()) {
            return Some(symbol);
        }

        if let Some(outer) = &self.outer {
            return outer.resolve(name);
        }

        None
    }

    pub fn length(&self) -> usize {
        self.store.len()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_define() {
        let expected = {
            let mut map = HashMap::new();
            map.insert(
                "a",
                Symbol {
                    name: "a".into(),
                    scope: Scope::Global,
                    index: 0,
                },
            );
            map.insert(
                "b",
                Symbol {
                    name: "b".into(),
                    scope: Scope::Global,
                    index: 1,
                },
            );
            map
        };

        let mut global = SymbolTable::new();

        assert_eq!(expected["a"], global.define("a"));

        assert_eq!(expected["b"], global.define("b"));
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = vec![
            Symbol {
                name: "a".into(),
                scope: Scope::Global,
                index: 0,
            },
            Symbol {
                name: "b".into(),
                scope: Scope::Global,
                index: 1,
            },
        ];

        for symbol in expected.iter() {
            assert_eq!(symbol, global.resolve(&symbol.name).unwrap());
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");
    }
}
