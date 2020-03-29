use monkey::lexer::Lexer;
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
        for token in lexer {
            println!("{:?}", token)
        }
    }
}
