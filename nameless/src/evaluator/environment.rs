use super::Object;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

pub type Env = Arc<Mutex<HashMap<String, Object>>>;

#[derive(Debug, Clone)]
pub struct Environment {
    store: Env,
    outer: Option<Box<Environment>>,
}

impl PartialEq for Environment {
    fn eq(&self, other: &Environment) -> bool {
        if self.compare_store(other) {
            return false;
        }

        match &self.outer {
            Some(outer) => {
                if other.outer.is_none() {
                    return false;
                }

                outer.compare_store(other.outer.as_ref().unwrap())
            }
            None => {
                if other.outer.is_some() {
                    return false;
                }

                true
            }
        }
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: Arc::new(Mutex::new(HashMap::new())),
            outer: None,
        }
    }

    pub fn compare_store(&self, other: &Environment) -> bool {
        let store = match self.store.lock() {
            Ok(env) => env,
            Err(_) => return false,
        };
        let other_store = match other.store.lock() {
            Ok(env) => env,
            Err(_) => return false,
        };

        if *store != *other_store {
            return false;
        }

        true
    }

    pub fn new_enclosed(outer: Environment) -> Environment {
        Environment {
            store: Arc::new(Mutex::new(HashMap::new())),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.lock() {
            Ok(env) => match env.get(name) {
                Some(obj) => Some(obj.clone()),
                None => match &self.outer {
                    Some(outer) => outer.get(name),
                    None => None,
                },
            },
            Err(_) => None,
        }
    }

    pub fn exists(&self, name: &str) -> bool {
        match self.store.lock() {
            Ok(env) => env.contains_key(name),
            Err(_) => false,
        }
    }

    pub fn set(&self, name: String, value: &Object) {
        if let Ok(mut env) = self.store.lock() {
            env.insert(name, value.clone());
        }
    }
}
