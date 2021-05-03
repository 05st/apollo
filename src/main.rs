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

use rustyline::error::ReadlineError;
use rustyline::Editor;
use colored::*;

#[derive(StructOpt, Debug)]
struct Application {
    #[structopt(short = "v", long = "verbose")]
    verbose: bool,
    #[structopt(short = "p", long = "pretend")]
    pretend: bool,
    #[structopt(short = "f", long = "file")]
    source: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut interpreter = Interpreter::new();
    let application = Application::from_args();
    if !atty::is(Stream::Stdin) {
        let mut parser = Parser::new(Lexer::new(io::stdin().lock().lines().next().unwrap()?));
        let root = parser.parse()?;
        if application.verbose { println!("{}", format!("{:#?}", root).blue()); }
        if !application.pretend { interpreter.interpret(root)?; }
    } else if let Some(file) = application.source {
        let mut parser = Parser::new(Lexer::new(fs::read_to_string(file)?));
        let root = parser.parse()?;
        if application.verbose { println!("{}", format!("{:#?}", root).blue()); }
        if !application.pretend { interpreter.interpret(root)?; }
    } else {
        let mut repl = Editor::<()>::new();
        loop {
            let read = repl.readline(&format!("{}", "> ".yellow()));
            match read {
                Ok(input) => {
                    repl.add_history_entry(&input);
                    let mut parser = Parser::new(Lexer::new(input));
                    match parser.parse() {
                        Ok(root) => {
                            if application.verbose { println!("{}", format!("{:#?}", root).blue()); }
                            if !application.pretend {
                                if let Err(msg) = interpreter.interpret(root) {
                                    println!("{}", msg.red());
                                }
                            }
                        },
                        Err(msg) => eprintln!("{}", msg.red()),
                    }
                },
                Err(ReadlineError::Interrupted) => break,
                _ => (),
            }
        }
    }
    Ok(())
}
