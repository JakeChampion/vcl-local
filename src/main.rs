#![allow(dead_code)]
extern crate clap;
extern crate derive_builder;

use clap::{App, Arg};

use std::fs;

mod expr;
mod parser;
mod scanner;

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
                        Ok(stmts) => {
                            if matches.is_present(SYNTAX_CHECK) {
                                println!("successfully parsed");
                                std::process::exit(0);
                            }
                            if matches.is_present(SHOW_AST_STR) {
                                println!("AST: {:#?}", stmts);
                                std::process::exit(0);
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
