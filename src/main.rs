#![feature(wrapping_int_impl)]
#![allow(dead_code)]
#![forbid(unsafe_code, future_incompatible, rust_2018_idioms)]
#![deny(missing_debug_implementations, nonstandard_style)]
#![warn(
    // clippy::all,
    // clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
)]

use clap::{App, Arg};

use std::fs;

mod expr;
mod parser;
mod scanner;
mod interpreter;
mod parse_probe;

static INPUT_STR: &str = "INPUT";
static SHOW_TOKENS_STR: &str = "show tokens";
static SHOW_AST_STR: &str = "show ast";
static SYNTAX_CHECK: &str = "check syntax";

fn main() {
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

    if let Some(input_file) = matches.value_of(INPUT_STR) {
        let maybe_input = fs::read_to_string(input_file);

        match maybe_input {
            Ok(input) => match scanner::scan_tokens(input) {
                Ok(tokens) => {
                    if matches.is_present(SHOW_TOKENS_STR) {
                        println!("tokens: {:#?}", tokens);
                        std::process::exit(0);
                    }

                    let stmts_maybe = parser::parse(tokens);

                    match stmts_maybe {
                        Ok(program) => {
                            if matches.is_present(SYNTAX_CHECK) {
                                println!("successfully parsed");
                                std::process::exit(0);
                            }
                            if matches.is_present(SHOW_AST_STR) {
                                println!("AST: {:#?}", program);
                                std::process::exit(0);
                            }

                            let mut interpreter: interpreter::Interpreter =
                                interpreter::Interpreter::default();
                            let interpret_result = interpreter.interpret(&program);

                            match interpret_result {
                                Ok(_) => {
                                    std::process::exit(0);
                                }
                                Err(err) => {
                                    println!(
                                        "Runtime Error: {}\n\n{}",
                                        err,
                                        interpreter.format_backtrace()
                                    );
                                    std::process::exit(-1);
                                }
                            }
                        }
                        Err(err) => {
                            println!("parse error: {:?}", err);
                            std::process::exit(-1)
                        }
                    }
                }
                Err(err) => {
                    println!("lexical error: {}", err);
                    std::process::exit(-1);
                }
            },
            Err(err) => {
                println!("Error reading {}: {}", input_file, err);
                std::process::exit(-1);
            }
        }
    }
}
