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
        let mut node = self.term()?;
        loop {
            match self.lexer.peek() {
                Token::Plus => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Add, Box::new(node), Box::new(self.term()?));
                },
                Token::Dash => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Add, Box::new(node), Box::new(self.term()?));
                },
                _ => break,
            }
        }
        Ok(node)
    }

    pub fn term(&mut self) -> Result<ASTNode, String> {
        let mut node = self.factor()?;
        loop {
            match self.lexer.peek() {
                Token::Asterisk => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Multiply, Box::new(node), Box::new(self.factor()?));
                },
                Token::Slash => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Divide, Box::new(node), Box::new(self.factor()?));
                },
                Token::Percent => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Modulo, Box::new(node), Box::new(self.factor()?));
                },
                _ => break,
            }
        }
        Ok(node)
    }

    pub fn factor(&mut self) -> Result<ASTNode, String> {
        let mut node = match self.lexer.peek() {
            Token::Plus => {
                self.lexer.next();
                ASTNode::Unary(Operator::Add, Box::new(self.factor()?))
            },
            Token::Dash => {
                self.lexer.next();
                ASTNode::Unary(Operator::Subtract, Box::new(self.factor()?))
            },
            _ => self.item()?,
        };
        loop {
            match self.lexer.peek() {
                Token::Caret => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Exponent, Box::new(node), Box::new(self.factor()?))
                },
                _ => break,
            }
        }
        Ok(node)
    }

    pub fn item(&mut self) -> Result<ASTNode, String> {
        let token = self.lexer.next();
        match token {
            Token::Number(x) => Ok(ASTNode::Number(x)),
            Token::Bool(x) => Ok(ASTNode::Bool(x)),
            Token::LeftParen => {
                let inside = self.parse()?;
                if let Token::RightParen = self.lexer.next() {
                    Ok(inside)
                } else {
                    Err(String::from("Expected RightParen"))
                }
            },
            _ => Err(format!("Unexpected Token {:?}", token))
        }
    }

    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
}
