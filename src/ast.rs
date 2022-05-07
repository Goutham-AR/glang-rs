#[derive(PartialEq, Debug)]
pub enum Statement {
    Default,
    Def(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

impl Default for Statement {
    fn default() -> Self {
        Statement::Default
    }
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Default,
    Ident(Identifier),
    Literal(Literal),
    Prefix(Operator, Box<Expression>),
    Infix(Box<Expression>, Operator, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Default,
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    EqualEqual,
    BangEqual,
    Greater,
    Lesser,
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Int(i64),
}

impl Default for Expression {
    fn default() -> Self {
        Expression::Default
    }
}

pub type ProgramBlock = Vec<Statement>;
pub type Program = ProgramBlock;

#[derive(PartialEq, Debug)]
pub struct Identifier {
    pub name: String,
}
