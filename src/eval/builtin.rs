use crate::eval::object::Object;
use crate::eval::EvalResult;
use std::cmp::PartialEq;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BuiltinFn {
    First,
    Last,
    Rest,

    Len,

    Puts,
}

impl BuiltinFn {
    pub fn call(&self, args: Vec<Object>) -> EvalResult {
        match &self {
            BuiltinFn::First => match &args[..] {
                [Object::Array(items)] => match items.first() {
                    Some(item) => Ok(item.clone()),
                    None => Ok(Object::Null),
                },
                _ => Err(format!("'first' expected an Array, got {:?}", args)),
            },
            BuiltinFn::Last => match &args[..] {
                [Object::Array(items)] => match items.last() {
                    Some(item) => Ok(item.clone()),
                    None => Ok(Object::Null),
                },
                _ => Err(format!("'last' expected an Array, got {:?}", args)),
            },
            BuiltinFn::Rest => match &args[..] {
                [Object::Array(items)] => {
                    let rest = items.iter().cloned().skip(1).collect::<Vec<Object>>();
                    Ok(Object::Array(rest))
                }
                _ => Err(format!("'rest' expected an Array, got {:?}", args)),
            },
            BuiltinFn::Len => match &args[..] {
                [Object::StringObj(s)] => Ok(Object::Int(s.len() as i64)),
                [Object::Array(items)] => Ok(Object::Int(items.len() as i64)),
                _ => Err(format!("'len' expected StringObj or Array, got {:?}", args)),
            },

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
            BuiltinFn::First => write!(f, "first"),
            BuiltinFn::Last => write!(f, "last"),
            BuiltinFn::Rest => write!(f, "rest"),
            BuiltinFn::Len => write!(f, "len"),
            BuiltinFn::Puts => write!(f, "puts"),
        }
    }
}
