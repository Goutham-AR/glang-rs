#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
pub enum TokenType {
    Default,
    Illegal,
    Eof,

    Identifier,
    Int,

    // operators
    Assign,
    Plus,
    Minus,
    Bang,
    Star,
    Slash,
    Lt,
    Gt,
    EqualEqual,
    BangEqual,

    Comma,
    SemiColon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Def,
    If,
    Else,
    Return,
    True,
    False,
}

#[derive(PartialEq, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            token_type: TokenType::Default,
            literal: String::new(),
        }
    }
}

impl Token {
    pub fn new(token_type: TokenType, token_literal: &str) -> Self {
        Token {
            token_type,
            literal: token_literal.to_string(),
        }
    }
}
