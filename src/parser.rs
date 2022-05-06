use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: Token::default(),
            peek_token: Token::default(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        self.curr_token = std::mem::take(&mut self.peek_token);
        // std::mem::swap(&mut self.curr_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Statement {
        match self.curr_token.token_type {
            TokenType::Def => self.parse_def_statement(),
            _ => Statement::Default,
        }
    }

    fn parse_def_statement(&mut self) -> Statement {
        if !self.expect_peek(TokenType::Identifier) {
            // panic!("Expected an identifier");
            return Statement::Default;
        }

        let ident_name = self.curr_token.literal.clone();

        if !self.expect_peek(TokenType::Assign) {
            // panic!("Expected '='");
            return Statement::Default;
        }

        let stmt = Statement::LetStatement(
            Identifier { name: ident_name },
            Expression::Literal(Literal::Int(
                self.peek_token.literal.parse::<i64>().unwrap(),
            )),
        );

        while !self.curr_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        stmt
    }

    fn curr_token_is(&mut self, ttype: TokenType) -> bool {
        self.curr_token.token_type == ttype
    }

    fn peek_token_is(&mut self, ttype: TokenType) -> bool {
        self.peek_token.token_type == ttype
    }

    fn expect_peek(&mut self, ttype: TokenType) -> bool {
        if self.peek_token_is(ttype) {
            self.next_token();
            true
        } else {
            false
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.curr_token.token_type != TokenType::Eof {
            let stmt = self.parse_statement();
            if stmt != Statement::Default {
                program.push(stmt);
            }
            self.next_token();
        }

        program
    }
}

#[cfg(test)]
mod parser_test {

    use crate::lexer::*;
    use crate::parser::*;

    #[test]
    fn parse_def_statement() {
        let input = "
def a = 3;
def y = 10;
def x = 12;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_program = vec![
            Statement::LetStatement(
                Identifier {
                    name: "a".to_string(),
                },
                Expression::Literal(Literal::Int(3)),
            ),
            Statement::LetStatement(
                Identifier {
                    name: "y".to_string(),
                },
                Expression::Literal(Literal::Int(10)),
            ),
            Statement::LetStatement(
                Identifier {
                    name: "x".to_string(),
                },
                Expression::Literal(Literal::Int(12)),
            ),
        ];

        // assert_eq!(
        //     program.len(),
        //     3,
        //     "The resulting program should contain 3 statement"
        // );

        assert_eq!(program, expected_program);
    }
}
