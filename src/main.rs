mod lexer;
mod parser;
mod interpreter;

use lexer::*;
use parser::*;
use interpreter::*;

use std::env;
use std::fs;

fn main() {
    let mut interpreter = Interpreter::new();

    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let input = fs::read_to_string(args[1].clone()).expect("Failed to read input file");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse() {
            Ok(root) => interpreter.interpret(root),
            Err(m) => println!("{}", m),
        }
    } else {
        loop {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();

            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            match parser.parse() {
                Ok(root) => interpreter.interpret(root),
                Err(m) => println!("{}", m),
            }
        }
    }
}
