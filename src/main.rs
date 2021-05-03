mod lexer;
mod parser;
mod interpreter;

use lexer::*;
use parser::*;
use interpreter::*;

use std::env;
use std::fs;
use std::path::{PathBuf};
use structopt::StructOpt;
use rustyline::Editor;

#[cfg(debug_assertions)]
macro_rules! dbg_quiet {
    () => {
        ::std::eprintln!("[{}:{}]", ::std::file!(), ::std::line!());
    };
    ($val:expr $(,)?) => {
        match $val {
            tmp => {
                ::std::eprintln!("[{}:{}] {} = {:#?}",
                    ::std::file!(), ::std::line!(), ::std::stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($(::std::dbg_quiet!($val)),+,)
    };
}

#[cfg(not(debug_assertions))]
macro_rules! dbg_quiet {
	    () => {};
    ($val:expr $(,)?) => {
		match $val {
			tmp => tmp
		}
    };
    ($($val:expr),+ $(,)?) => {
        ($(::std::dbg_quiet!($val)),+,)
    };
}

#[derive(StructOpt, Debug)]
#[structopt(name = "apollo")]
struct Application {
	#[structopt(short, long, parse(from_os_str))]
	file: Option<PathBuf>,
	#[structopt(name = "EXPRESSION")]
	expr: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let application = Application::from_args();
	let mut interpreter = Interpreter::new();

	if let Some(file) = application.file {
		let input = fs::read_to_string(&file).expect("Failed to read input file");
		let lexer = Lexer::new(input);
		let mut parser = Parser::new(lexer);
		match dbg_quiet! { parser.parse() } {
			Ok(root) => interpreter.interpret(root),
			Err(m) => eprintln!("{}", m),
		}

	} else if let Some(expr) = application.expr {
		let lexer = Lexer::new(expr);
		let mut parser = Parser::new(lexer);
		interpreter.interpret(dbg_quiet! { parser.parse() }?);
	}
	else {
		let mut rl = Editor::<()>::new();
		loop {
			let mut source = rl.readline("> ")?;
			rl.add_history_entry(&source);
			let lexer = Lexer::new(source);
			let mut parser = Parser::new(lexer);
			match dbg_quiet! { parser.parse() } {
				Ok(root) => interpreter.interpret(root),
				Err(m) => eprintln!("{}", m),
			}
		}
	}
	Ok(())
}
