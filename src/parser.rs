use crate::lexer::*;

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Number(f64),
    Bool(bool),
    Unary(Operator, Box<ASTNode>),
    Binary(Operator, Box<ASTNode>, Box<ASTNode>),
    Variable(String),
    Compound(Vec<ASTNode>),
    Assign(Box<ASTNode>, Box<ASTNode>),
    Empty,
}

pub struct Parser {
    lexer: Lexer
}

impl Parser {
    pub fn parse(&mut self) -> Result<ASTNode, String> {
        self.program()
    }

    pub fn program(&mut self) -> Result<ASTNode, String> {
        Ok(self.compound_statement()?)
    }

    pub fn compound_statement(&mut self) -> Result<ASTNode, String> {
        let list = match self.lexer.peek() {
            Token::LeftBrace => {
                self.lexer.next();
                let statements = self.statement_list()?;
                if let Token::RightBrace = self.lexer.next() {
                    statements
                } else {
                    return Err(String::from("Expected RightBrace"));
                }
            }
            _ => return Err(String::from("Expected LeftBrace")),
        };
        Ok(ASTNode::Compound(list))
    }

    pub fn statement_list(&mut self) -> Result<Vec<ASTNode>, String> {
        let mut list = vec![self.statement()?];
        loop {
            match self.lexer.peek() {
                Token::Semicolon => {
                    self.lexer.next();
                    list.push(self.statement()?);
                },
                _ => break,
            }
        }
        Ok(list)
    }

    pub fn statement(&mut self) -> Result<ASTNode, String> {
        match self.lexer.peek() {
            Token::LeftBrace => Ok(self.compound_statement()?),
            Token::Identifier(_) => Ok(self.assignment_statement()?),
            _ => Ok(ASTNode::Empty),
        }
    }

    pub fn assignment_statement(&mut self) -> Result<ASTNode, String> {
        if let Token::Identifier(v) = self.lexer.next() {
            if let Token::Equal = self.lexer.next() {
                Ok(ASTNode::Assign(Box::new(ASTNode::Variable(v)), Box::new(self.expression()?)))
            } else {
                Err(String::from("Expected Equal"))
            }
        } else {
            Err(String::from("Expected Identifier"))
        }
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
                    node = ASTNode::Binary(Operator::Subtract, Box::new(node), Box::new(self.term()?));
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
                let inside = self.expression()?;
                if let Token::RightParen = self.lexer.next() {
                    Ok(inside)
                } else {
                    Err(String::from("Expected RightParen"))
                }
            },
            Token::Identifier(x) => Ok(ASTNode::Variable(x)),
            _ => Err(format!("Unexpected Token {:?}", token))
        }
    }

    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
}
