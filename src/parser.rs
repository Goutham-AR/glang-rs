use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

use std::collections::HashMap;

type PrefixParseFn = fn(&mut Parser) -> Expression;
type InfixParseFn = fn(&mut Parser, Expression) -> Expression;

#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
enum Precedence {
    Lowest,
    Equals,     // ==
    Comparison, // > or <
    Sum,
    Product,
    Prefix,
    Call,
}

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    // error: String,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
    prec_table: HashMap<TokenType, Precedence>,
}

impl Parser {
    fn parse_identifier(&mut self) -> Expression {
        Expression::Ident(Identifier {
            name: self.curr_token.literal.to_string(),
        })
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let value: i64 = match self.curr_token.literal.parse() {
            Ok(value) => value,
            Err(err) => {
                panic!("Parse Error: {}", err);
            }
        };

        Expression::Literal(Literal::Int(value))
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let operator = match self.curr_token.token_type {
            TokenType::Minus => Operator::Minus,
            TokenType::Bang => Operator::Bang,
            _ => {
                // panic!("Invalid prefix operator");
                Operator::Default
            }
        };

        self.next_token();
        let expr = Box::new(self.parse_expression(Precedence::Prefix));

        Expression::Prefix(operator, expr)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let prec = self.curr_prec();

        let operator = match self.curr_token.token_type {
            TokenType::Plus => Operator::Plus,
            TokenType::Minus => Operator::Minus,
            TokenType::Slash => Operator::Slash,
            TokenType::Star => Operator::Star,
            TokenType::EqualEqual => Operator::EqualEqual,
            TokenType::BangEqual => Operator::BangEqual,
            TokenType::Lt => Operator::Lesser,
            TokenType::Gt => Operator::Greater,
            _ => Operator::Default,
        };

        self.next_token();
        let right = Box::new(self.parse_expression(prec));

        Expression::Infix(Box::new(left), operator, right)
    }

    fn parse_boolean(&mut self) -> Expression {
        let value = self.curr_token.token_type == TokenType::True;
        Expression::Literal(Literal::Bool(value))
    }

    fn parse_grouping(&mut self) -> Expression {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            panic!("Expected ')' after '('");
        }

        expr
    }

    fn parse_block_statement(&mut self) -> ProgramBlock {
        self.next_token();

        let mut stmts = vec![];

        while !self.curr_token_is(TokenType::RBrace) && !self.curr_token_is(TokenType::Eof) {
            let stmt = self.parse_statement();
            if stmt != Statement::Default {
                stmts.push(stmt);
            }
            self.next_token();
        }

        stmts
    }

    fn parse_if(&mut self) -> Expression {
        if !self.expect_peek(TokenType::LParen) {
            panic!("'(' not found for if expression");
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            println!("{:?}", condition);
            panic!("')' not found for if expression");
        }

        if !self.expect_peek(TokenType::LBrace) {
            panic!("'{{' not found for if expression");
        }

        let consequence = self.parse_block_statement();

        if self.peek_token_is(TokenType::Else) {
            self.next_token();
            if !self.expect_peek(TokenType::LBrace) {
                panic!("Expected '{{' for else");
            }

            let alternative = self.parse_block_statement();

            return Expression::If(Box::new(condition), consequence, alternative);
        }

        Expression::If(Box::new(condition), consequence, vec![Statement::Default])
    }

    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: Token::default(),
            peek_token: Token::default(),
            // error: String::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            prec_table: HashMap::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
            .prec_table
            .insert(TokenType::EqualEqual, Precedence::Equals);
        parser
            .prec_table
            .insert(TokenType::BangEqual, Precedence::Equals);
        parser
            .prec_table
            .insert(TokenType::Lt, Precedence::Comparison);
        parser
            .prec_table
            .insert(TokenType::Gt, Precedence::Comparison);
        parser.prec_table.insert(TokenType::Plus, Precedence::Sum);
        parser.prec_table.insert(TokenType::Minus, Precedence::Sum);
        parser
            .prec_table
            .insert(TokenType::Slash, Precedence::Product);
        parser
            .prec_table
            .insert(TokenType::Star, Precedence::Product);

        parser.register_prefix_fn(TokenType::Identifier, Parser::parse_identifier);
        parser.register_prefix_fn(TokenType::Int, Parser::parse_integer_literal);
        parser.register_prefix_fn(TokenType::Bang, Parser::parse_prefix_expression);
        parser.register_prefix_fn(TokenType::Minus, Parser::parse_prefix_expression);
        parser.register_prefix_fn(TokenType::True, Parser::parse_boolean);
        parser.register_prefix_fn(TokenType::False, Parser::parse_boolean);
        parser.register_prefix_fn(TokenType::LParen, Parser::parse_grouping);
        parser.register_prefix_fn(TokenType::If, Parser::parse_if);

        parser.register_infix_fn(TokenType::Plus, Parser::parse_infix_expression);
        parser.register_infix_fn(TokenType::Minus, Parser::parse_infix_expression);
        parser.register_infix_fn(TokenType::Star, Parser::parse_infix_expression);
        parser.register_infix_fn(TokenType::Slash, Parser::parse_infix_expression);
        parser.register_infix_fn(TokenType::EqualEqual, Parser::parse_infix_expression);
        parser.register_infix_fn(TokenType::BangEqual, Parser::parse_infix_expression);
        parser.register_infix_fn(TokenType::Lt, Parser::parse_infix_expression);
        parser.register_infix_fn(TokenType::Gt, Parser::parse_infix_expression);

        parser
    }

    fn peek_prec(&self) -> Precedence {
        if let Some(prec) = self.prec_table.get(&self.peek_token.token_type) {
            *prec
        } else {
            // panic!("Precedence not found");
            Precedence::Lowest
        }
    }

    fn curr_prec(&self) -> Precedence {
        if let Some(prec) = self.prec_table.get(&self.curr_token.token_type) {
            *prec
        } else {
            // panic!("Precedence not found");
            Precedence::Lowest
        }
    }

    pub fn register_prefix_fn(&mut self, ttype: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(ttype, func);
    }

    pub fn register_infix_fn(&mut self, ttype: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(ttype, func);
    }

    pub fn next_token(&mut self) {
        self.curr_token = std::mem::take(&mut self.peek_token);
        // std::mem::swap(&mut self.curr_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Statement {
        match self.curr_token.token_type {
            TokenType::Def => self.parse_def_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_def_statement(&mut self) -> Statement {
        if !self.expect_peek(TokenType::Identifier) {
            // self.error = format!("Expected an identifier");
            return Statement::Default;
        }

        let ident_name = self.curr_token.literal.clone();

        if !self.expect_peek(TokenType::Assign) {
            // panic!("Expected '='");
            return Statement::Default;
        }

        let expr = self.peek_token.literal.parse::<i64>().unwrap();

        let stmt = Statement::Def(
            Identifier { name: ident_name },
            Expression::Literal(Literal::Int(expr)),
        );

        while !self.curr_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        stmt
    }

    fn parse_return_statement(&mut self) -> Statement {
        let expr: i64 = self.peek_token.literal.parse().unwrap();

        let stmt = Statement::Return(Expression::Literal(Literal::Int(expr)));

        while !self.curr_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        stmt
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expr = self.parse_expression(Precedence::Lowest);
        let stmt = Statement::Expression(expr);

        if self.peek_token_is(TokenType::SemiColon) {
            self.next_token();
        }

        stmt
    }

    fn parse_expression(&mut self, prec: Precedence) -> Expression {
        let mut left_exp;
        if let Some(&prefix_fn) = self.prefix_parse_fns.get(&self.curr_token.token_type) {
            left_exp = prefix_fn(self);
        } else {
            return Expression::Default;
        }

        while !self.peek_token_is(TokenType::SemiColon) && prec < self.peek_prec() {
            if let Some(&infix_fn) = self.infix_parse_fns.get(&self.peek_token.token_type) {
                self.next_token();
                left_exp = infix_fn(self, left_exp);
            } else {
                return left_exp;
            }
        }

        left_exp
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

        while !self.curr_token_is(TokenType::Eof) {
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

    use std::vec;

    use crate::lexer::*;
    use crate::parser::*;

    fn program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    #[test]
    fn test_def_statement() {
        let input = "
def a = 3;
def y = 10;
def x = 12;";
        let program = program(input);

        let expected_program = vec![
            Statement::Def(
                Identifier {
                    name: "a".to_string(),
                },
                Expression::Literal(Literal::Int(3)),
            ),
            Statement::Def(
                Identifier {
                    name: "y".to_string(),
                },
                Expression::Literal(Literal::Int(10)),
            ),
            Statement::Def(
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

    #[test]
    fn test_return_statement() {
        let input = "
return 3;
return 12;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_program = vec![
            Statement::Return(Expression::Literal(Literal::Int(3))),
            Statement::Return(Expression::Literal(Literal::Int(12))),
        ];

        assert_eq!(program, expected_program);
    }

    #[test]
    fn test_ident_expression() {
        let input = "hello";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_program = vec![Statement::Expression(Expression::Ident(Identifier {
            name: input.to_string(),
        }))];

        assert_eq!(program, expected_program);
    }

    #[test]
    fn test_int_expression() {
        let input = "10";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_program = vec![Statement::Expression(Expression::Literal(Literal::Int(10)))];

        assert_eq!(program, expected_program);
    }

    #[test]
    fn test_prefix_expression() {
        let input = "
-10;
!14;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_program = vec![
            Statement::Expression(Expression::Prefix(
                Operator::Minus,
                Box::new(Expression::Literal(Literal::Int(10))),
            )),
            Statement::Expression(Expression::Prefix(
                Operator::Bang,
                Box::new(Expression::Literal(Literal::Int(14))),
            )),
        ];

        assert_eq!(program, expected_program);
    }

    #[test]
    fn test_infix_expression() {
        let input = "
6 + 6;
6 - 6;
6 * 6;
6 / 6;
6 < 6;
6 > 6;
6 == 6;
6 != 6;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expr = Box::new(Expression::Literal(Literal::Int(6)));

        let expected_program = vec![
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::Plus,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::Minus,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::Star,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::Slash,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::Lesser,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::Greater,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::EqualEqual,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
            Statement::Expression(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(6))),
                Operator::BangEqual,
                Box::new(Expression::Literal(Literal::Int(6))),
            )),
        ];

        assert_eq!(program, expected_program);
    }

    // #[test]
    // fn test_literal_expression() {
    //     todo!()
    // }

    #[test]
    fn test_operator_precedence() {
        let input = "
(5 + 5) * 4;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_program = vec![Statement::Expression(Expression::Infix(
            Box::new(Expression::Infix(
                Box::new(Expression::Literal(Literal::Int(5))),
                Operator::Plus,
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Operator::Star,
            Box::new(Expression::Literal(Literal::Int(4))),
        ))];

        assert_eq!(program, expected_program);
    }

    #[test]
    fn test_boolean() {
        let input = "
false
true";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let expected_program = vec![
            Statement::Expression(Expression::Literal(Literal::Bool(false))),
            Statement::Expression(Expression::Literal(Literal::Bool(true))),
        ];

        assert_eq!(program, expected_program);
    }

    #[test]
    fn test_if() {
        let input = "
if (x > y) { x } else { y }";

        let program = program(input);

        let expected_program = vec![Statement::Expression(Expression::If(
            Box::new(Expression::Infix(
                Box::new(Expression::Ident(Identifier {
                    name: "x".to_string(),
                })),
                Operator::Greater,
                Box::new(Expression::Ident(Identifier {
                    name: "y".to_string(),
                })),
            )),
            vec![Statement::Expression(Expression::Ident(Identifier {
                name: "x".to_string(),
            }))],
            vec![Statement::Expression(Expression::Ident(Identifier {
                name: "y".to_string(),
            }))],
        ))];

        assert_eq!(program, expected_program);
    }
}

/*
def a = 3;
def sum = fn(a, b) {
    return a + b;
};



*/
