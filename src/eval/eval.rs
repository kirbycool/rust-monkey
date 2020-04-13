use crate::eval::object::Object::*;
use crate::eval::object::{Env, EnvWrapper, Object};
use crate::parser::ast::{Expr, Program, Stmt};
use crate::parser::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

pub type EvalResult = Result<Object, String>;

pub fn eval(program: Program, env: EnvWrapper) -> EvalResult {
    let mut result = Null;
    for stmt in program.statements.into_iter() {
        result = eval_stmt(stmt, env.clone())?;
        if let Return(obj) = result {
            return Ok(*obj);
        }
    }

    Ok(result)
}

pub fn eval_stmts(stmts: Vec<Stmt>, env: EnvWrapper) -> EvalResult {
    let mut result = Null;
    for stmt in stmts.into_iter() {
        result = eval_stmt(stmt, env.clone())?;
        if let Return(_) = result {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_stmt(stmt: Stmt, env: EnvWrapper) -> EvalResult {
    match stmt {
        Stmt::BlockStmt(block) => eval_stmts(block, env),
        Stmt::Expr(expr) => eval_expr(expr, env),
        Stmt::Return(expr) => Ok(Return(Box::new(eval_expr(expr, env)?))),
        Stmt::Let(name, expr) => {
            let value = eval_expr(expr, env.clone())?;
            env.borrow_mut().insert(name, value.clone());
            Ok(value)
        }
    }
}

fn eval_expr(expr: Expr, env: EnvWrapper) -> EvalResult {
    match expr {
        Expr::Int(value) => Ok(Int(value)),
        Expr::Bool(value) => Ok(Bool(value)),
        Expr::Prefix(token, expr) => eval_prefix(token, eval_expr(*expr, env.clone())?),
        Expr::Infix(left, op, right) => eval_infix(
            eval_expr(*left, env.clone())?,
            op,
            eval_expr(*right, env.clone())?,
        ),
        Expr::If(cond, cons, alt) => eval_conditional(
            eval_expr(*cond, env.clone())?,
            *cons,
            alt.map(|e| *e),
            env.clone(),
        ),
        Expr::Ident(name) => env
            .borrow()
            .get(&name)
            .map(|obj| obj.clone())
            .ok_or(format!("Unbound identifier: {}", name)),
        Expr::FunctionLiteral(params, body) => Ok(Function {
            params,
            body: *body,
            env,
        }),
        Expr::Call(func, params) => eval_function_call(*func, params, env.clone()),
    }
}

fn eval_prefix(token: Token, right: Object) -> EvalResult {
    match token {
        Token::Bang => match right {
            Bool(value) => Ok(Bool(!value)),
            Null => Ok(Bool(false)),
            _ => Ok(Bool(true)),
        },
        Token::Minus => match right {
            Int(value) => Ok(Int(-value)),
            _ => Err(format!("Can't negate \"{}\"", right.to_string())),
        },
        token => Err(format!("Illegal prefix token {}", token)),
    }
}

fn eval_infix(left: Object, op: Token, right: Object) -> EvalResult {
    match (left, right) {
        (Int(l), Int(r)) => eval_int_infix(l, op, r),
        (Bool(l), Bool(r)) => eval_bool_infix(l, op, r),
        (l, r) => Err(format!(
            "Illegal operation {} {} {}",
            l.to_string(),
            op.to_string(),
            r.to_string(),
        )),
    }
}

fn eval_int_infix(left: i64, op: Token, right: i64) -> EvalResult {
    match op {
        Token::Plus => Ok(Int(left + right)),
        Token::Minus => Ok(Int(left - right)),
        Token::Asterisk => Ok(Int(left * right)),
        Token::Slash => Ok(Int(left / right)),

        Token::LessThan => Ok(Bool(left < right)),
        Token::GreaterThan => Ok(Bool(left > right)),
        Token::Equal => Ok(Bool(left == right)),
        Token::NotEqual => Ok(Bool(left != right)),
        token => Err(format!("Illegal infix token {}", token)),
    }
}

fn eval_bool_infix(left: bool, op: Token, right: bool) -> EvalResult {
    match op {
        Token::LessThan => Ok(Bool(left < right)),
        Token::GreaterThan => Ok(Bool(left > right)),
        Token::Equal => Ok(Bool(left == right)),
        Token::NotEqual => Ok(Bool(left != right)),
        token => Err(format!("Illegal infix token {}", token)),
    }
}

fn eval_conditional(cond: Object, cons: Stmt, alt: Option<Stmt>, env: EnvWrapper) -> EvalResult {
    let cond = cond != Bool(false) && cond != Null;

    if cond {
        eval_stmt(cons, env)
    } else {
        match alt {
            Some(stmt) => eval_stmt(stmt, env),
            None => Ok(Null),
        }
    }
}

fn eval_function_call(func_expr: Expr, args: Vec<Expr>, env: EnvWrapper) -> EvalResult {
    let func = eval_expr(func_expr, env.clone())?;
    let (params, body, func_env) = match func {
        Function { params, body, env } => (params, body, env),
        _ => return Err(format!("Can't call: {}", func)),
    };

    let args = args
        .into_iter()
        .map(|expr| eval_expr(expr, env.clone()))
        .collect::<Result<Vec<Object>, String>>()?;

    let mut call_env = Env::from(func_env);
    for (param, arg) in params.into_iter().zip(args.into_iter()) {
        let _ = match param {
            Expr::Ident(name) => call_env.insert(name, arg),
            _ => return Err(format!("Invalid param: {}", param)),
        };
    }

    let result = eval_stmt(body, Rc::new(RefCell::new(call_env)))?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::eval::object::Env;
    use crate::eval::object::Object::*;
    use crate::eval::{eval, EvalResult};
    use crate::parser::ast::{Expr, Stmt};
    use crate::parser::token::Token;
    use crate::parser::{Lexer, Parser};
    use std::cell::RefCell;
    use std::rc::Rc;

    fn assert_cases(cases: Vec<(&str, EvalResult)>) -> () {
        for (input, output) in cases.iter().cloned() {
            let mut parser = Parser::new(Lexer::new(input.to_string()));
            let program = parser.parse().unwrap();
            assert_eq!(eval(program, Rc::new(RefCell::new(Env::new()))), output)
        }
    }

    #[test]
    fn int_expr() {
        let cases = vec![("5", Ok(Int(5))), ("10", Ok(Int(10)))];
        assert_cases(cases)
    }

    #[test]
    fn bool_expr() {
        let cases = vec![("true", Ok(Bool(true))), ("false", Ok(Bool(false)))];
        assert_cases(cases)
    }

    #[test]
    fn bang() {
        let cases = vec![
            ("!true", Ok(Bool(false))),
            ("!false", Ok(Bool(true))),
            ("!!true", Ok(Bool(true))),
            ("!4", Ok(Bool(true))),
            ("!!4", Ok(Bool(false))),
        ];
        assert_cases(cases)
    }

    #[test]
    fn negate() {
        let cases = vec![
            ("-5", Ok(Int(-5))),
            ("--10", Ok(Int(10))),
            ("-true", Err("Can't negate \"true\"".to_string())),
        ];
        assert_cases(cases)
    }

    #[test]
    fn add() {
        let cases = vec![("1 + 2", Ok(Int(3))), ("3 + 4 + 5", Ok(Int(12)))];
        assert_cases(cases)
    }

    #[test]
    fn subtract() {
        let cases = vec![("1 - 2", Ok(Int(-1))), ("10 - 2 - 3", Ok(Int(5)))];
        assert_cases(cases)
    }

    #[test]
    fn multiply() {
        let cases = vec![("1 * 2", Ok(Int(2))), ("3 * 4 * 5", Ok(Int(60)))];
        assert_cases(cases)
    }

    #[test]
    fn divide() {
        let cases = vec![("1 / 2", Ok(Int(0))), ("60 / 3 / 4", Ok(Int(5)))];
        assert_cases(cases)
    }

    #[test]
    fn combined_arithmetic() {
        let cases = vec![("1 + 2 * 3 - 4 / 2", Ok(Int(5)))];
        assert_cases(cases)
    }

    #[test]
    fn int_comparison() {
        let cases = vec![
            ("1 < 2", Ok(Bool(true))),
            ("2 < 1", Ok(Bool(false))),
            ("2 > 1", Ok(Bool(true))),
            ("1 > 1", Ok(Bool(false))),
            ("1 == 1", Ok(Bool(true))),
            ("1 != 1", Ok(Bool(false))),
            ("1 == 2", Ok(Bool(false))),
            ("1 != 2", Ok(Bool(true))),
        ];
        assert_cases(cases)
    }

    #[test]
    fn bool_comparison() {
        let cases = vec![
            ("true == true", Ok(Bool(true))),
            ("false == false", Ok(Bool(true))),
            ("true == false", Ok(Bool(false))),
            ("true != false", Ok(Bool(true))),
            ("false != true", Ok(Bool(true))),
            ("(1 < 2) == true", Ok(Bool(true))),
            ("(1 < 2) == false", Ok(Bool(false))),
            ("(1 > 2) == true", Ok(Bool(false))),
            ("(1 > 2) == false", Ok(Bool(true))),
        ];
        assert_cases(cases)
    }

    #[test]
    fn conditional() {
        let cases = vec![
            ("if (true) { 10 }", Ok(Int(10))),
            ("if (false) { 10 }", Ok(Null)),
            ("if (1) { 10 }", Ok(Int(10))),
            ("if (1 < 2) { 10 }", Ok(Int(10))),
            ("if (1 > 2) { 10 }", Ok(Null)),
            ("if (1 > 2) { 10 } else { 20 }", Ok(Int(20))),
            ("if (1 < 2) { 10 } else { 20 }", Ok(Int(10))),
        ];
        assert_cases(cases)
    }

    #[test]
    fn return_stmt() {
        let cases = vec![
            ("return 10;", Ok(Int(10))),
            ("return 10; 9;", Ok(Int(10))),
            ("return 2 * 5; 9;", Ok(Int(10))),
            ("9; return 2 * 5; 9;", Ok(Int(10))),
            (
                "
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }
             ",
                (Ok(Int(10))),
            ),
        ];
        assert_cases(cases)
    }

    #[test]
    fn let_expr() {
        let cases = vec![
            ("let a = 5; a;", Ok(Int(5))),
            ("let a = 5 * 5; a;", Ok(Int(25))),
            ("let a = 5; let b = a; b;", Ok(Int(5))),
            ("let a = 5; let b = a; let c = a + b + 5; c;", Ok(Int(15))),
            ("a;", Err("Unbound identifier: a".to_string())),
        ];
        assert_cases(cases)
    }

    #[test]
    fn function_def() {
        let cases = vec![(
            "fn(x) { x + 2; }",
            Ok(Function {
                params: vec![Expr::Ident("x".to_string())],
                body: Stmt::BlockStmt(vec![Stmt::Expr(Expr::Infix(
                    Box::new(Expr::Ident("x".to_string())),
                    Token::Plus,
                    Box::new(Expr::Int(2)),
                ))]),
                env: Rc::new(RefCell::new(Env::new())),
            }),
        )];
        assert_cases(cases)
    }

    #[test]
    fn function_application() {
        let cases = vec![
            ("let identity = fn(x) { x; }; identity(5);", Ok(Int(5))),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Ok(Int(5)),
            ),
            ("let double = fn(x) { x * 2; }; double(5);", Ok(Int(10))),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Ok(Int(10))),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Ok(Int(20)),
            ),
            ("fn(x) { x; }(5)", Ok(Int(5))),
        ];
        assert_cases(cases)
    }

    #[test]
    fn closure() {
        let cases = vec![
            (
                "let x = 5; let add_x = fn(y) { x + y; }; add_x(3);",
                Ok(Int(8)),
            ),
            (
                "let x = 5; let add = fn(x, y) { x + y; }; add(3, 1);",
                Ok(Int(4)),
            ),
            (
                "let x = 5; let add_2 = fn(y) { let x = 2; x + y; }; add_2(3);",
                Ok(Int(5)),
            ),
        ];
        assert_cases(cases)
    }
}
