mod lexer;
mod parser;

use lexer::*;
use parser::*;

fn main() {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input);
        println!("{:?}", lexer);
    }
}
