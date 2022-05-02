use std::io;
use std::io::prelude::*;

use crate::lexer;

const PROMPT: &str = ">>";

pub fn start() -> io::Result<()> {
    loop {
        print!("{}", PROMPT);
        for line_result in io::stdin().lock().lines() {
            let line = line_result?;
            let lexer = lexer::Lexer::new(&line);

            for token in lexer {
                println!("{:?}", token);
            }
        }
    }
}
