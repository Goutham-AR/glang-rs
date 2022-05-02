use crate::token::*;

use std::collections::HashMap;
use std::iter::Iterator;

pub struct Lexer {
    source_code: Vec<u8>,
    current_position: usize,
    read_position: usize,
    current_char: u8,
    keywords: HashMap<&'static str, TokenType>,
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.token_type != TokenType::Eof {
            Some(token)
        } else {
            None
        }
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            source_code: Vec::from(input.as_bytes()),
            current_position: 0,
            read_position: 1,
            current_char: input.as_bytes()[0],
            keywords: HashMap::from([
                ("fn", TokenType::Function),
                ("def", TokenType::Def),
                ("true", TokenType::True),
                ("false", TokenType::False),
                ("if", TokenType::If),
                ("else", TokenType::Else),
                ("return", TokenType::Return),
            ]),
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.source_code.len() {
            self.current_char = 0;
        } else {
            self.current_char = self.source_code[self.read_position];
        }

        self.current_position = self.read_position;
        self.read_position += 1;
    }

    fn peek_next(&self) -> u8 {
        if (self.read_position >= self.source_code.len()) {
            0
        } else {
            self.source_code[self.read_position]
        }
    }

    fn is_letter(&self, ch: u8) -> bool {
        b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z' || ch == b'_'
    }

    fn look_up_keyword(&self, keyword: &str) -> TokenType {
        match self.keywords.get(keyword) {
            Some(t) => *t,
            None => TokenType::Identifier,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.current_char {
                b' ' | b'\t' | b'\n' | b'\r' => {
                    self.read_char();
                }
                _ => {
                    break;
                }
            }
        }
    }

    fn read_indentifier(&mut self) -> String {
        let pos = self.current_position;
        while self.is_letter(self.current_char) {
            self.read_char();
        }

        std::str::from_utf8(&self.source_code[pos..self.current_position])
            .unwrap()
            .to_string()
    }

    fn read_number(&mut self) -> String {
        let pos = self.current_position;
        while self.current_char.is_ascii_digit() {
            self.read_char();
        }
        return std::str::from_utf8(&self.source_code[pos..self.current_position])
            .unwrap()
            .to_string();
    }

    pub fn next_token(&mut self) -> Token {
        let mut token = Token::default();

        self.skip_whitespace();

        match self.current_char {
            b'=' => {
                if self.peek_next() == b'=' {
                    // let ch = self.current_char;
                    self.read_char();
                    token = Token::new(TokenType::EqualEqual, "==");
                } else {
                    token = Token::new(TokenType::Assign, "=");
                }
            }
            b'+' => token = Token::new(TokenType::Plus, "+"),
            b'-' => token = Token::new(TokenType::Minus, "-"),
            b'!' => {
                if self.peek_next() == b'=' {
                    // let ch = self.current_char;
                    self.read_char();
                    token = Token::new(TokenType::BangEqual, "!=");
                } else {
                    token = Token::new(TokenType::Bang, "!")
                }
            }
            b'/' => token = Token::new(TokenType::Slash, "/"),
            b'*' => token = Token::new(TokenType::Star, "*"),
            b'>' => token = Token::new(TokenType::Gt, ">"),
            b'<' => token = Token::new(TokenType::Lt, "<"),
            b'(' => token = Token::new(TokenType::LParen, "("),
            b')' => token = Token::new(TokenType::RParen, ")"),
            b'{' => token = Token::new(TokenType::LBrace, "{"),
            b'}' => token = Token::new(TokenType::RBrace, "}"),
            b',' => token = Token::new(TokenType::Comma, ","),
            b';' => token = Token::new(TokenType::SemiColon, ";"),
            b'\0' => token = Token::new(TokenType::Eof, "EOF"),
            _ => {
                if self.is_letter(self.current_char) {
                    token.literal = self.read_indentifier();
                    token.token_type = self.look_up_keyword(&token.literal);
                    return token;
                } else if self.current_char.is_ascii_digit() {
                    token.token_type = TokenType::Int;
                    token.literal = self.read_number();
                    return token;
                } else {
                    token = Token::new(TokenType::Illegal, "illegal token");
                }
            }
        }

        self.read_char();
        token
    }
}

#[cfg(test)]
mod lexer_test {

    use crate::lexer::*;
    use crate::token::*;

    #[test]
    fn test_next_token() {
        let input = "def five = 5;
def ten = 10;
def add = fn(x, y) {
x + y;
};
def result = add(five, ten);
!-/*5;
5 < 10 > 5
if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";

        let expected_tokens = [
            Token::new(TokenType::Def, "def"),
            Token::new(TokenType::Identifier, "five"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::Def, "def"),
            Token::new(TokenType::Identifier, "ten"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::Def, "def"),
            Token::new(TokenType::Identifier, "add"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Function, "fn"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Identifier, "x"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Identifier, "y"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Identifier, "x"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Identifier, "y"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::Def, "def"),
            Token::new(TokenType::Identifier, "result"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Identifier, "add"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Identifier, "five"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Identifier, "ten"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::Bang, "!"),
            Token::new(TokenType::Minus, "-"),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Star, "*"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Gt, ">"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::If, "if"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Lt, "<"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::True, "true"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Else, "else"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::False, "false"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::EqualEqual, "=="),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::SemiColon, ";"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::BangEqual, "!="),
            Token::new(TokenType::Int, "9"),
            Token::new(TokenType::SemiColon, ";"),
        ];

        //         !-/*5;
        // 5 < 10 > 5
        // if (5 < 10) {
        //     return true;
        // } else {
        //     return false;
        // }

        let mut lexer = Lexer::new(input);

        for token in expected_tokens {
            let t = lexer.next_token();

            assert_eq!(t, token);
        }
    }
}
