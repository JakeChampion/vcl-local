#![feature(wrapping_int_impl)]
#![allow(dead_code)]
#![forbid(unsafe_code, future_incompatible, rust_2018_idioms)]
#![deny(missing_debug_implementations, nonstandard_style)]
// #![warn(
//         // clippy::all,
//         // clippy::restriction,
//         clippy::pedantic,
//         clippy::nursery,
//     )]
use clap::{App, Arg};

use std::fs;

mod expr;
mod interpreter;
mod parser;
mod scanner;

static INPUT_STR: &str = "INPUT";
static SHOW_TOKENS_STR: &str = "show tokens";
static SHOW_AST_STR: &str = "show ast";
static SYNTAX_CHECK: &str = "check syntax";
// static mut interpreter: interpreter::Interpreter = interpreter::Interpreter::default();

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let matches = App::new("vcl")
        .version("0.1.0")
        .about("vcl language interpreter")
        .author("Jake Champion")
        .arg(
            Arg::with_name(INPUT_STR)
                .help("sets input file to use")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name(SHOW_TOKENS_STR)
                .long("--show-tokens")
                .takes_value(false)
                .help("show the token stream"),
        )
        .arg(
            Arg::with_name(SHOW_AST_STR)
                .long("--show-ast")
                .takes_value(false)
                .help("show the AST"),
        )
        .arg(
            Arg::with_name(SYNTAX_CHECK)
                .long("--syntax-check")
                .takes_value(false)
                .help("check the syntax can be parsed"),
        )
        .get_matches();

    let input_file = matches.value_of(INPUT_STR).expect("require input file");
    let input = fs::read_to_string(input_file).expect("Error reading input file");

    let tokens = scanner::scan_tokens(input);
    let only_tokenize = matches.is_present(SHOW_TOKENS_STR);
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(err) => {
            println!("lexical error: {}", err);
            std::process::exit(-1);
        }
    };
    if only_tokenize {
        println!("tokens: {:#?}", tokens);
        std::process::exit(0);
    }

    let program = match parser::parse(tokens) {
        Ok(program) => program,
        Err(err) => {
            println!("parse error: {:?}", err);
            std::process::exit(-1);
        }
    };

    if matches.is_present(SYNTAX_CHECK) {
        println!("successfully parsed");
        std::process::exit(0);
    }
    if matches.is_present(SHOW_AST_STR) {
        println!("AST: {:#?}", program);
        std::process::exit(0);
    }

    // lazy_static!{
    //     static mut interpreter: interpreter::Interpreter = interpreter::Interpreter::default();
    // }

    let mut interpreter: interpreter::Interpreter = interpreter::Interpreter::default();
    interpreter.interpret(program).await
}
