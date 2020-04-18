use crate::eval::builtin::BuiltinFn;
use crate::eval::object::Object::*;
use crate::eval::object::{Env, EnvWrapper, Object};
use crate::parser::ast::{Expr, Program, Stmt};
use crate::parser::token::Token;

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
        Expr::String(value) => Ok(StringObj(value)),
        Expr::Array(items) => {
            let item_objs = items
                .into_iter()
                .map(|expr| eval_expr(expr, env.clone()))
                .collect::<Result<Vec<Object>, String>>()?;
            Ok(Array(item_objs))
        }
        Expr::Index { left, index } => eval_index(*left, *index, env),
        Expr::Prefix { op, right } => eval_prefix(op, eval_expr(*right, env.clone())?),
        Expr::Infix { left, op, right } => eval_infix(
            eval_expr(*left, env.clone())?,
            op,
            eval_expr(*right, env.clone())?,
        ),
        Expr::If {
            test,
            consequent,
            alternative,
        } => eval_conditional(
            eval_expr(*test, env.clone())?,
            *consequent,
            alternative.map(|e| *e),
            env.clone(),
        ),
        Expr::Ident(name) => eval_ident(name, env),
        Expr::FunctionLiteral { params, body } => Ok(Function {
            params,
            body: *body,
            env,
        }),
        Expr::Call { func, args } => eval_function_call(*func, args, env.clone()),
    }
}

fn eval_index(left: Expr, index: Expr, env: EnvWrapper) -> EvalResult {
    let index_obj = eval_expr(index, env.clone())?;
    match eval_expr(left, env)? {
        Array(items) => match index_obj {
            Int(index) => match items.get(index as usize) {
                Some(item) => Ok(item.clone()),
                None => Ok(Null),
            },
            obj => Err(format!("'{}' is not a valid array index", obj)),
        },
        obj => Err(format!("Can't index object '{}'", obj.to_string())),
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
        (StringObj(l), StringObj(r)) => Ok(StringObj(format!("{}{}", l, r))),
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

fn eval_ident(name: String, env: EnvWrapper) -> EvalResult {
    match name.as_str() {
        "first" => Ok(Builtin(BuiltinFn::First)),
        "last" => Ok(Builtin(BuiltinFn::Last)),
        "len" => Ok(Builtin(BuiltinFn::Len)),
        "puts" => Ok(Builtin(BuiltinFn::Puts)),
        _ => {
            let obj = env
                .borrow()
                .get(&name)
                .ok_or(format!("Unbound identifier: {}", name));
            obj.clone()
        }
    }
}

fn eval_function_call(func_expr: Expr, args: Vec<Expr>, env: EnvWrapper) -> EvalResult {
    let args = args
        .into_iter()
        .map(|expr| eval_expr(expr, env.clone()))
        .collect::<Result<Vec<Object>, String>>()?;

    let func = eval_expr(func_expr, env.clone())?;
    match func {
        Function { params, body, env } => {
            let mut call_env = Env::from(env);
            for (param, arg) in params.into_iter().zip(args.into_iter()) {
                let _ = match param {
                    Expr::Ident(name) => call_env.insert(name, arg),
                    _ => return Err(format!("Invalid param: {}", param)),
                };
            }

            eval_stmt(body, call_env.wrap())
        }
        Builtin(builtin_fn) => builtin_fn.call(args),
        _ => Err(format!("Can't call: {}", func)),
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::object::Env;
    use crate::eval::object::Object::*;
    use crate::eval::{eval, EvalResult};
    use crate::parser::ast::{Expr, Stmt};
    use crate::parser::token::Token;
    use crate::parser::{Lexer, Parser};

    fn assert_cases(cases: Vec<(&str, EvalResult)>) -> () {
        for (input, output) in cases.iter().cloned() {
            let mut parser = Parser::new(Lexer::new(input.to_string()));
            let program = parser.parse().unwrap();
            assert_eq!(eval(program, Env::new().wrap()), output)
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
    fn string_expr() {
        let cases = vec![
            ("\"foo\"", Ok(StringObj("foo".to_string()))),
            ("\"\"", Ok(StringObj("".to_string()))),
        ];
        assert_cases(cases)
    }

    #[test]
    fn string_concat() {
        let cases = vec![("\"foo\" + \"bar\"", Ok(StringObj("foobar".to_string())))];
        assert_cases(cases)
    }

    #[test]
    fn array() {
        let cases = vec![(
            "[1, 1 + 2, \"foo\" + \"bar\"]",
            Ok(Array(vec![Int(1), Int(3), StringObj("foobar".to_string())])),
        )];
        assert_cases(cases)
    }

    #[test]
    fn array_index() {
        let cases = vec![
            ("let foo = [1, 1 + 2]; foo[1];", Ok(Int(3))),
            ("[1, 1 + 2][0];", Ok(Int(1))),
            ("[1, 1 + 2][2];", Ok(Null)),
        ];
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
                body: Stmt::BlockStmt(vec![Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Ident("x".to_string())),
                    op: Token::Plus,
                    right: Box::new(Expr::Int(2)),
                })]),
                env: Env::new().wrap(),
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

    #[test]
    fn builtin_first() {
        let cases = vec![
            ("first([1, 2])", Ok(Int(1))),
            ("first([])", Ok(Null)),
            (
                "first(1)",
                Err("'first' expected an Array, got [Int(1)]".to_string()),
            ),
        ];
        assert_cases(cases)
    }

    #[test]
    fn builtin_last() {
        let cases = vec![
            ("last([1, 2])", Ok(Int(2))),
            ("last([])", Ok(Null)),
            (
                "last(1)",
                Err("'last' expected an Array, got [Int(1)]".to_string()),
            ),
        ];
        assert_cases(cases)
    }

    #[test]
    fn builtin_len() {
        let cases = vec![
            ("len(\"\")", Ok(Int(0))),
            ("len(\"four\")", Ok(Int(4))),
            ("len([1, 2])", Ok(Int(2))),
            (
                "len(1)",
                Err("'len' expected StringObj or Array, got [Int(1)]".to_string()),
            ),
            (
                "len(\"foo\", \"bar\")",
                Err(
                    r#"'len' expected StringObj or Array, got [StringObj("foo"), StringObj("bar")]"#
                        .to_string(),
                ),
            ),
        ];
        assert_cases(cases)
    }

    #[test]
    fn builtin_puts() {
        let cases = vec![("puts(\"Foo\")", Ok(Null)), ("puts(1, 2)", Ok(Null))];
        assert_cases(cases)
    }
}
