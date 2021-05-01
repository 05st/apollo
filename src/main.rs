mod lexer;
mod parser;
mod interpreter;

use lexer::*;
use parser::*;
use interpreter::*;

fn main() {
    let mut interpreter = Interpreter::new();

    let code = "
        let test = 10;
        let y = 5;
        {
            let test = 5;
            y = 2;
            print(test - y);
        }
        print(test);
        print(y);
    ";

    println!("{}", interpreter.interpret(Parser::new(Lexer::new(code.to_string())).parse().unwrap()));

    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input);
        // println!("{:?}", lexer.clone());
        let mut parser = Parser::new(lexer);
        match parser.parse() {
            Ok(root) => {
                // println!("{:?}", root);
                println!("{}", interpreter.interpret(root));
            },
            Err(m) => println!("{}", m),
        }
    }
}
