use crate::eval::object::Object;
use crate::eval::EvalResult;
use std::cmp::PartialEq;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinFn {
    Len,
    Puts,
}

impl BuiltinFn {
    pub fn call(&self, args: Vec<Object>) -> EvalResult {
        match &self {
            BuiltinFn::Len => {
                if let [Object::StringObj(s)] = &args[..] {
                    Ok(Object::Int(s.len() as i64))
                } else {
                    Err(format!("Expected StringObj, got {:?}", args))
                }
            }
            BuiltinFn::Puts => {
                for arg in args.iter() {
                    println!("{}", arg);
                }
                Ok(Object::Null)
            }
        }
    }
}

impl fmt::Display for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            BuiltinFn::Len => write!(f, "len"),
            BuiltinFn::Puts => write!(f, "puts"),
        }
    }
}
