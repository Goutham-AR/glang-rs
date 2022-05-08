use std::io;
use std::io::prelude::*;

use crate::lexer;
use crate::parser;
use crate::token::TokenType;

const PROMPT: &str = ">>";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        for line_result in io::stdin().lock().lines() {
            let line = line_result.expect("Failed to read line");
            let lexer = lexer::Lexer::new(&line);
            let mut parser = parser::Parser::new(lexer);
            // for token in lexer {
            //     println!("{:?}", token);
            // }

            let program = parser.parse_program();

            for stmt in program {
                println!("{:?}", stmt);
            }
        }
    }
}
