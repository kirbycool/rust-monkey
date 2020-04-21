use crate::parser::token::Token;
use std::fmt;
use std::hash::Hash;

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let strings: Vec<String> = self
            .statements
            .iter()
            .map(|statement| format!("{};", statement.to_string()))
            .collect();

        write!(f, "{}", strings.join("\n"))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Stmt {
    Let(String, Expr),
    Return(Expr),
    Expr(Expr),
    BlockStmt(Vec<Stmt>),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Let(name, value) => write!(f, "let {} = {}", name, value.to_string()),
            Stmt::Return(expr) => write!(f, "return {}", expr.to_string()),
            Stmt::Expr(expr) => write!(f, "{}", expr.to_string()),
            Stmt::BlockStmt(statements) => {
                let strings: Vec<String> = statements
                    .iter()
                    .map(|statement| format!("{};", statement.to_string()))
                    .collect();
                write!(f, "{}", strings.join("\n"))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expr {
    Ident(String),
    Int(i64),
    Bool(bool),
    String(String),
    Prefix {
        op: Token,
        right: Box<Expr>,
    },
    Infix {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    If {
        test: Box<Expr>,
        consequent: Box<Stmt>,
        alternative: Option<Box<Stmt>>,
    },
    FunctionLiteral {
        params: Vec<Expr>,
        body: Box<Stmt>,
    },
    Array(Vec<Expr>),
    Hash(Vec<(Expr, Expr)>),
    Index {
        left: Box<Expr>,
        index: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::Int(value) => write!(f, "{}", value.to_string()),
            Expr::Bool(value) => write!(f, "{}", value.to_string()),
            Expr::String(value) => write!(f, "{}", value),
            Expr::Prefix { op, right } => write!(f, "({}{})", op.to_string(), right.to_string()),
            Expr::Infix { left, op, right } => write!(
                f,
                "({} {} {})",
                left.to_string(),
                op.to_string(),
                right.to_string()
            ),
            Expr::If {
                test,
                consequent,
                alternative,
            } => match alternative {
                Some(alt) => write!(
                    f,
                    "if ({}) {{\n{}\n}} else {{\n{}\n}}",
                    test.to_string(),
                    indent(consequent.to_string().as_str(), 1),
                    indent(alt.to_string().as_str(), 1)
                ),
                None => write!(
                    f,
                    "if ({}) {{\n{}\n}}",
                    test.to_string(),
                    indent(consequent.to_string().as_str(), 1)
                ),
            },
            Expr::Array(items) => write!(
                f,
                "[{}]",
                items
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Expr::Hash(hash) => write!(
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
            Expr::Index { left, index } => write!(f, "{}[{}]", left.to_string(), index.to_string()),
            Expr::FunctionLiteral { params, body } => write!(
                f,
                "fn ({}) {{\n{}\n}}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                indent(body.to_string().as_str(), 1)
            ),
            Expr::Call { func, args } => write!(
                f,
                "{}({})",
                func,
                args.iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expr, Program, Stmt};

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![
                Stmt::Let(String::from("foo"), Expr::Ident(String::from("bar"))),
                Stmt::Let(String::from("baz"), Expr::Ident(String::from("foo"))),
            ],
        };
        let expected = String::from(
            "\
let foo = bar;
let baz = foo;",
        );
        assert_eq!(program.to_string(), expected)
    }
}

pub fn indent(input: &str, level: usize) -> String {
    let indented = input.lines().map(|line| {
        if line.chars().any(|c| !c.is_whitespace()) {
            format!("{}{}", "    ".repeat(level), line)
        } else {
            line.to_string()
        }
    });
    indented.collect::<Vec<String>>().join("\n")
}
