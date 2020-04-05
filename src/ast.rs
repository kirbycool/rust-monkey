use crate::token::Token;
use std::fmt;

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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Ident(String),
    Int(i32),
    Bool(bool),
    Prefix(Token, Box<Expr>),
    Infix(Box<Expr>, Token, Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    FunctionLiteral(Vec<Expr>, Box<Stmt>),
    Call(Box<Expr>, Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Ident(name) => write!(f, "{}", name),
            Expr::Int(value) => write!(f, "{}", value.to_string()),
            Expr::Bool(value) => write!(f, "{}", value.to_string()),
            Expr::Prefix(operator, operand) => {
                write!(f, "({}{})", operator.to_string(), operand.to_string())
            }
            Expr::Infix(left, operator, right) => write!(
                f,
                "({} {} {})",
                left.to_string(),
                operator.to_string(),
                right.to_string()
            ),
            Expr::If(condition, consequence, alternative) => match alternative {
                Some(alt) => write!(
                    f,
                    "if ({}) {{\n{}\n}} else {{\n{}\n}}",
                    condition.to_string(),
                    indent(consequence.to_string().as_str(), 1),
                    indent(alt.to_string().as_str(), 1)
                ),
                None => write!(
                    f,
                    "if ({}) {{\n{}\n}}",
                    condition.to_string(),
                    indent(consequence.to_string().as_str(), 1)
                ),
            },
            Expr::FunctionLiteral(params, body) => write!(
                f,
                "fn ({}) {{\n{}\n}}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                indent(body.to_string().as_str(), 1)
            ),
            Expr::Call(name, args) => write!(
                f,
                "{}({})",
                name,
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
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Program, Stmt};

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

fn indent(input: &str, level: usize) -> String {
    let indented = input.lines().map(|line| {
        if line.chars().any(|c| !c.is_whitespace()) {
            format!("{}{}", "    ".repeat(level), line)
        } else {
            line.to_string()
        }
    });
    indented.collect::<Vec<String>>().join("\n")
}
