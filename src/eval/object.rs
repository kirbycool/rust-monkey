use crate::eval::builtin::BuiltinFn;
use crate::parser::ast::{indent, Expr, Stmt};
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// These should probably be their own structs. Doing this is too inflexible.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    StringObj(String),
    Array(Vec<Object>),
    Return(Box<Object>),
    Function {
        params: Vec<Expr>,
        body: Stmt,
        env: EnvWrapper,
    },
    Builtin(BuiltinFn),
    Hash(HashMap<Object, Object>),
    Null,
}

impl Object {
    pub fn is_hash_key(&self) -> bool {
        match self {
            Object::Int(_) | Object::Bool(_) | Object::StringObj(_) => true,
            _ => false,
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Int(v) => v.hash(state),
            Object::StringObj(v) => v.hash(state),
            Object::Bool(v) => v.hash(state),
            _ => "unsupported".hash(state),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}", value.to_string()),
            Object::Bool(value) => write!(f, "{}", value.to_string()),
            Object::StringObj(value) => write!(f, "{}", value),
            Object::Array(items) => write!(
                f,
                "[{}]",
                items
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Return(value) => write!(f, "{}", value.to_string()),
            Object::Function { params, body, .. } => write!(
                f,
                "fn ({}) {{\n{}\n}}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                indent(body.to_string().as_str(), 1)
            ),
            Object::Builtin(func) => write!(f, "{}", func),
            Object::Hash(hash) => write!(
                f,
                "{{\n{}\n}}",
                indent(
                    hash.iter()
                        .map(|(k, v)| format!("{}: {},", k.to_string(), v.to_string()))
                        .collect::<Vec<String>>()
                        .join("\n")
                        .as_str(),
                    1
                )
            ),
            Object::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<EnvWrapper>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn from(outer: EnvWrapper) -> Self {
        Env {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn wrap(self) -> EnvWrapper {
        Rc::new(RefCell::new(self))
    }

    pub fn get(&self, key: &String) -> Option<Object> {
        let value = self.store.get(key);
        match value {
            Some(_) => value.cloned(),
            None => self.outer.as_ref().and_then(|env| env.borrow().get(key)),
        }
    }

    pub fn insert(&mut self, key: String, value: Object) -> Option<Object> {
        self.store.insert(key, value)
    }
}

pub type EnvWrapper = Rc<RefCell<Env>>;

#[cfg(test)]
mod tests {
    use crate::eval::object::{Env, Object};

    #[test]
    fn env() {
        let mut env = Env::new();
        env.insert("foo".to_string(), Object::Int(10));
        env.insert("bar".to_string(), Object::Null);

        assert_eq!(env.get(&"foo".to_string()), Some(Object::Int(10)));
        assert_eq!(env.get(&"bar".to_string()), Some(Object::Null));
    }

    #[test]
    fn env_closure() {
        let mut outer = Env::new();
        outer.insert("foo".to_string(), Object::Int(10));
        outer.insert("bar".to_string(), Object::Null);

        let mut env = Env::from(outer.wrap());
        env.insert("bar".to_string(), Object::Int(5));
        env.insert("baz".to_string(), Object::Int(2));

        assert_eq!(env.get(&"foo".to_string()), Some(Object::Int(10)));
        assert_eq!(env.get(&"bar".to_string()), Some(Object::Int(5)));
        assert_eq!(env.get(&"baz".to_string()), Some(Object::Int(2)));
    }
}
