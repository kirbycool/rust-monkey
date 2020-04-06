use crate::eval::object::Object;
use crate::eval::object::Object::*;
use crate::parser::ast::{Expr, Program, Stmt};

pub fn eval(program: Program) -> Object {
    let mut result = Null;
    for stmt in program.statements.into_iter() {
        result = eval_stmt(stmt);
    }
    result
}

fn eval_stmt(stmt: Stmt) -> Object {
    match stmt {
        Stmt::Expr(expr) => eval_expr(expr),
        _ => Null,
    }
}

fn eval_expr(expr: Expr) -> Object {
    match expr {
        Expr::Int(value) => Int(value),
        Expr::Bool(value) => Bool(value),
        _ => Null,
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::eval;
    use crate::eval::object::Object;
    use crate::eval::object::Object::*;
    use crate::parser::ast::Expr;
    use crate::parser::{Lexer, Parser};

    fn assert_cases(cases: Vec<(&str, Object)>) -> () {
        for (input, output) in cases.iter().cloned() {
            let mut parser = Parser::new(Lexer::new(input.to_string()));
            let program = parser.parse().unwrap();
            assert_eq!(eval(program), output)
        }
    }

    #[test]
    fn int_expr() {
        let cases = vec![("5", Int(5)), ("10", Int(10))];
        assert_cases(cases)
    }

    #[test]
    fn bool_expr() {
        let cases = vec![("true", Bool(true)), ("false", Bool(false))];
        assert_cases(cases)
    }
}
