#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(value) => value.to_string(),
            Object::Bool(value) => value.to_string(),
            Object::Null => "null".to_string(),
        }
    }
}
