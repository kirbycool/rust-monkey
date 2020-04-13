use crate::eval::eval;
use crate::eval::object::{Env, EnvWrapper};
use crate::parser::{Lexer, Parser};
use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

pub fn repl_loop() {
    println!("Welcome to a super great Monkey REPL");
    println!("Type .exit to exit.");
    println!("Type .help to get a syntax error. I am lazy.");

    let env = Rc::new(RefCell::new(Env::new()));

    loop {
        print!(">> ");
        io::stdout().flush().expect("Failed to write prompt");

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read");

        read_command(&input);
        eval_code(input, env.clone())
    }
}

fn read_command(input: &String) -> () {
    match input.as_str() {
        ".exit\n" => std::process::exit(0),
        _ => (),
    }
}

fn eval_code(input: String, env: EnvWrapper) -> () {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    match parser.parse() {
        Ok(program) => match eval(program, env) {
            Ok(obj) => println!("{}", obj.to_string()),
            Err(error) => println!("{}", error.to_string()),
        },
        Err(errors) => println!("{}", errors.join("\n")),
    }
}
