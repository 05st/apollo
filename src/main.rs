mod lexer;
mod parser;
mod interpreter;

use lexer::*;
use parser::*;
use interpreter::*;

use std::fs;
use std::path::PathBuf;

use structopt::StructOpt;
use atty::Stream;
use std::io::BufRead;
use std::io;

#[derive(StructOpt, Debug)]
struct Application {
    #[structopt(short = "v", long = "verbose")]
    verbose: bool,
    #[structopt(short = "p", long = "pretend")]
    pretend: bool,
    #[structopt(short = "f", long = "source")]
    source: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut interpreter = Interpreter::new();
    let application = Application::from_args();
    if !atty::is(Stream::Stdin) {
        let lexer = Lexer::new(io::stdin().lock().lines().next().unwrap()?);
        let mut parser = Parser::new(lexer);
        let root = if application.verbose { dbg!{ parser.parse()? } } else { parser.parse()? };
        if !application.pretend {
            interpreter.interpret(root);
        }
    } else if let Some(file) = application.source {
        let source = fs::read_to_string(file)?;
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        let root = if application.verbose { dbg!{ parser.parse()? } } else { parser.parse()? };
        if !application.pretend {
            interpreter.interpret(root);
        }
    }
    Ok(())
}
