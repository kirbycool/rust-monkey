use crate::token::Token;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut strings = Vec::new();
        let mut iter = self.statements.iter().peekable();
        while let Some(statement) = iter.next() {
            if iter.peek().is_some() {
                strings.push(format!("{};", statement.to_string()))
            } else {
                strings.push(format!("{}", statement.to_string()))
            }
        }
        write!(f, "{}", strings.join("\n"))
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(name, value) => write!(f, "let {} = {}", name, value.to_string()),
            Statement::Return(expr) => write!(f, "return {}", expr.to_string()),
            Statement::Expression(expr) => write!(f, "{}", expr.to_string()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Int(i32),
    Prefix(Token, Box<Expression>),
    Infix(Box<Expression>, Token, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(name) => write!(f, "{}", name),
            Expression::Int(value) => write!(f, "{}", value.to_string()),
            Expression::Prefix(operator, operand) => {
                write!(f, "({}{})", operator.to_string(), operand.to_string())
            }
            Expression::Infix(left, operator, right) => write!(
                f,
                "({} {} {})",
                left.to_string(),
                operator.to_string(),
                right.to_string()
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
    use crate::ast::{Expression, Program, Statement};

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![
                Statement::Let(
                    String::from("foo"),
                    Expression::Identifier(String::from("bar")),
                ),
                Statement::Let(
                    String::from("baz"),
                    Expression::Identifier(String::from("foo")),
                ),
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
