use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Scope {
    Global,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: Scope,
    pub index: u32,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    count: u32,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
            count: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.into(),
            index: self.count,
            scope: Scope::Global,
        };

        if let Some(symbol) = self.resolve(name) {
            return symbol.clone();
        }

        self.store.insert(name.into(), symbol.clone());
        self.count += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name.into())
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
}
