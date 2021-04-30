use crate::lexer::*;

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Equal,
    Not,
    NotEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Number(f64),
    Bool(bool),
    Str(String),
    Binary(Operator, Box<ASTNode>, Box<ASTNode>),
    Unary(Operator, Box<ASTNode>),
    VarDecl(String, Box<Option<ASTNode>>),
    Write(Box<ASTNode>),
    ExprStmt(Box<ASTNode>),
    Variable(String),
    Compound(Vec<ASTNode>),
    Assign(String, Box<ASTNode>),
}

type RASTNode = Result<ASTNode, String>;

pub struct Parser {
    lexer: Lexer
}

impl Parser {
    fn expect(&mut self, token: Token) -> Result<Token, String> {
        let next = self.lexer.next();
        if next == token {
            Ok(next)
        } else {
            Err(format!("Expected {:?}", token))
        }
    }

    fn program(&mut self) -> RASTNode {
        let mut declarations = Vec::new();
        loop {
            match self.lexer.peek() {
                Token::EOF => break,
                _ => declarations.push(self.declaration()?),
            }
        }
        self.expect(Token::EOF)?;
        Ok(ASTNode::Compound(declarations))
    }

    fn declaration(&mut self) -> RASTNode {
        match self.lexer.peek() {
            Token::Let => Ok(self.var_decl()?),
            _ => Ok(self.statement()?),
        }
    }

    fn var_decl(&mut self) -> RASTNode {
        self.expect(Token::Let)?;
        if let Token::Identifier(id) = self.lexer.next() {
            let node = ASTNode::VarDecl(id, Box::new({
                if let Token::Equal = self.lexer.peek() {
                    self.lexer.next();
                    Option::Some(self.expression()?)
                } else {
                    Option::None
                }
            }));
            self.expect(Token::Semicolon)?;
            Ok(node)
        } else {
            Err(String::from("Expected Identifier"))
        }
    }

    fn statement(&mut self) -> RASTNode {
        match self.lexer.peek() {
            Token::LeftBrace => Ok(self.block()?),
            Token::Write => Ok(self.write_stmt()?),
            _ => Ok(self.expr_stmt()?),
        }
    }

    fn block(&mut self) -> RASTNode {
        self.expect(Token::LeftBrace)?;
        let mut declarations = Vec::new();
        loop {
            match self.lexer.peek() {
                Token::RightBrace => break,
                _ => declarations.push(self.declaration()?),
            }
        }
        self.expect(Token::RightBrace)?;
        Ok(ASTNode::Compound(declarations))
    }

    fn write_stmt(&mut self) -> RASTNode {
        self.expect(Token::Write)?;
        self.expect(Token::LeftParen)?;
        let node = ASTNode::Write(Box::new(self.expression()?));
        self.expect(Token::RightParen)?;
        self.expect(Token::Semicolon)?;
        Ok(node)
    }

    fn expr_stmt(&mut self) -> RASTNode {
        let expr = self.expression()?;
        self.expect(Token::Semicolon)?;
        Ok(ASTNode::ExprStmt(Box::new(expr)))
    }

    fn expression(&mut self) -> RASTNode {
        match self.lexer.peek() {
            Token::Identifier(_) => Ok(self.assignment()?),
            _ => Ok(self.equality()?),
        }
    }

    fn assignment(&mut self) -> RASTNode {
        if let Token::Identifier(id) = self.lexer.next() {
            self.expect(Token::Equal)?;
            Ok(ASTNode::Assign(id, Box::new(self.expression()?)))
        } else {
            Err(String::from("Expected Identifier"))
        }
    }

    fn equality(&mut self) -> RASTNode {
        let mut node = self.comparison()?;
        loop {
            match self.lexer.peek() {
                Token::NotEqual => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::NotEqual, Box::new(node), Box::new(self.comparison()?));
                },
                Token::EqualEqual => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Equal, Box::new(node), Box::new(self.comparison()?));
                },
                _ => break,
            }
        }
        Ok(node)
    }

    fn comparison(&mut self) -> RASTNode {
        let mut node = self.term()?;
        loop {
            match self.lexer.peek() {
                Token::Greater => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Greater, Box::new(node), Box::new(self.term()?));
                },
                Token::GreaterEqual => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::GreaterEqual, Box::new(node), Box::new(self.term()?));
                },
                Token::Lesser => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Lesser, Box::new(node), Box::new(self.term()?));
                },
                Token::LesserEqual => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::LesserEqual, Box::new(node), Box::new(self.term()?));
                },
                _ => break,
            }
        }
        Ok(node)
    }

    fn term(&mut self) -> RASTNode {
        let mut node = self.factor()?;
        loop {
            match self.lexer.peek() {
                Token::Plus => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Add, Box::new(node), Box::new(self.factor()?));
                },
                Token::Dash => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Subtract, Box::new(node), Box::new(self.factor()?));
                },
                _ => break,
            }
        }
        Ok(node)
    }

    fn factor(&mut self) -> RASTNode {
        let mut node = self.unary()?;
        loop {
            match self.lexer.peek() {
                Token::Asterisk => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Multiply, Box::new(node), Box::new(self.unary()?));
                },
                Token::Slash => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Divide, Box::new(node), Box::new(self.unary()?));
                },
                Token::Percent => {
                    self.lexer.next();
                    node = ASTNode::Binary(Operator::Modulo, Box::new(node), Box::new(self.unary()?));
                },
                _ => break,
            }
        }
        Ok(node)
    }

    fn unary(&mut self) -> RASTNode {
        match self.lexer.peek() {
            Token::Not => {
                self.lexer.next();
                Ok(ASTNode::Unary(Operator::Not, Box::new(self.unary()?)))
            },
            Token::Dash => {
                self.lexer.next();
                Ok(ASTNode::Unary(Operator::Subtract, Box::new(self.unary()?)))
            },
            _ => Ok(self.item()?),
        }
    }

    fn item(&mut self) -> RASTNode {
        match self.lexer.next() {
            Token::Number(x) => Ok(ASTNode::Number(x)),
            Token::Bool(x) => Ok(ASTNode::Bool(x)),
            Token::Str(x) => Ok(ASTNode::Str(x)),
            Token::LeftParen => {
                let expr = self.expression()?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            },
            Token::Identifier(x) => Ok(ASTNode::Variable(x)),
            _ => Err(String::from("Invalid item token"))
        }
    }

    pub fn parse(&mut self) -> RASTNode {
        self.program()
    }

    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
}
