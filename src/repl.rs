use std::io;
use std::io::prelude::*;

use crate::lexer;
use crate::token::TokenType;

const PROMPT: &str = ">>";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        for line_result in io::stdin().lock().lines() {
            let line = line_result.expect("Failed to read line");
            let mut lexer = lexer::Lexer::new(&line);

            for token in lexer {
                println!("{:?}", token);
            }

            // loop {
            //     let token = lexer.next_token();
            //     println!("{:?}", token);
            //     if token.token_type == TokenType::Eof {
            //         break;
            //     }
            // }
        }
    }
}
