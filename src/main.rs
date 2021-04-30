mod lexer;
mod parser;
mod interpreter;

use lexer::*;
use parser::*;
use interpreter::*;

fn main() {
    let code = "
        {
            w = 3.2 + 1.618^(4.8/2) * 8 % 3;
            x = w;
            y = x * 2;
            {
                z = x^2 + y;
            };
            w = 10;
        }
    ";

    Interpreter::new(
        Parser::new(
            Lexer::new(code.to_string())
        ).parse().unwrap()
    ).interpret();

    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse() {
            Ok(root) => {
                // println!("{:?}", root);
                let mut interpreter = Interpreter::new(root);
                interpreter.interpret();
            },
            Err(m) => println!("{}", m),
        }
    }
}
