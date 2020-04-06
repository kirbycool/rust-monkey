use crate::eval::eval;
use crate::parser::{Lexer, Parser};
use std::io;
use std::io::Write;

pub fn repl_loop() {
    println!("Welcome to a super great Monkey REPL");
    println!("Type .exit to exit.");
    println!("Type .help to get a syntax error. I am lazy.");

    loop {
        print!(">> ");
        io::stdout().flush().expect("Failed to write prompt");

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read");

        read_command(&input);
        eval_code(input)
    }
}

fn read_command(input: &String) -> () {
    match input.as_str() {
        ".exit\n" => std::process::exit(0),
        _ => (),
    }
}

fn eval_code(input: String) -> () {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    match parser.parse() {
        Ok(program) => println!("{}", eval(program).inspect()),
        Err(errors) => println!("{}", errors.join("\n")),
    }
}
