mod eval;

pub mod builtin;
pub mod object;

pub use eval::{eval, EvalResult};
