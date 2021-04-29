use crate::lexer::*;

#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,
}

#[derive(Debug)]
pub enum ASTNode {
    Number(f64),
    Bool(bool),
    Unary(Operator, Box<ASTNode>),
    Binary(Operator, Box<ASTNode>, Box<ASTNode>),
}

pub struct Parser {
    lexer: Lexer
}

impl Parser {
    pub fn parse(&mut self) -> Result<ASTNode, String> {
        self.expression()
    }

    pub fn expression(&mut self) -> Result<ASTNode, String> {
        
    }

    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
}
