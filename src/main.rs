mod lexer;

use lexer::*;

fn main() {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        println!("{:?}", tokenize(input));
    }
}
