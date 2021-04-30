mod lexer;
mod parser;
mod interpreter;

use lexer::*;
use parser::*;
use interpreter::*;

fn main() {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse() {
            Ok(root) => {
                let mut interpreter = Interpreter::new(root);
                interpreter.interpret();
            },
            Err(m) => println!("{}", m),
        }
    }
}
