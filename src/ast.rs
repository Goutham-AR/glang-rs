#[derive(PartialEq, Debug)]
pub enum Statement {
    Default,
    LetStatement(Identifier, Expression),
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
