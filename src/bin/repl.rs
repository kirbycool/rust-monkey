use monkey::lexer::Lexer;
use monkey::parser::Parser;
use std::io;
use std::io::Write;

static PROMPT: &str = ">> ";

fn main() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().expect("Failed to write prompt");

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse() {
            Ok(program) => println!("{}", program),
            Err(errors) => println!("{}", errors.join("\n")),
        }
    }
}
