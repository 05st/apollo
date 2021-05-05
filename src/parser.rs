use crate::lexer::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,

    Equal,
    Not,
    NotEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,

    LogicOr,
    LogicAnd,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    Compound(Vec<ASTNode>),
    Block(Vec<ASTNode>),
    ExprStmt(Box<ASTNode>),
    VarDecl(String, Box<Option<ASTNode>>),
    Function(String, Vec<String>, Box<ASTNode>),
    If(Box<ASTNode>, Box<ASTNode>, Box<Option<ASTNode>>),
    While(Box<ASTNode>, Box<ASTNode>),
    Break,
    Continue,
    Return(Box<ASTNode>),
    Assign(String, Box<ASTNode>),
    Binary(Operator, Box<ASTNode>, Box<ASTNode>),
    Unary(Operator, Box<ASTNode>),
    Call(String, Vec<ASTNode>),
    Variable(String),
    Number(f64),
    Bool(bool),
    String(String),
    Null,
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
            Token::Def => Ok(self.func_decl()?),
            _ => Ok(self.statement()?),
        }
    }

    fn var_decl(&mut self) -> RASTNode {
        self.expect(Token::Let)?;
        if let Token::Identifier(id) = self.lexer.next() {
            let node = ASTNode::VarDecl(id, Box::new({
                if let Token::Equal = self.lexer.peek() {
                    self.lexer.next();
                    Option::Some(self.equality()?)
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

    fn func_decl(&mut self) -> RASTNode {
        self.expect(Token::Def)?;
        if let Token::Identifier(id) = self.lexer.next() {
            self.expect(Token::LeftParen)?;
            let params = self.params()?;
            self.expect(Token::RightParen)?;
            Ok(ASTNode::Function(id, params, Box::new(self.block()?)))
        } else {
            Err(String::from("Expected Identifier"))
        }
    }

    fn params(&mut self) -> Result<Vec<String>, String> {
        let mut list = vec![];
        match self.lexer.peek() {
            Token::RightParen => (),
            _ => {
                if let ASTNode::Variable(id) = self.item()? {
                    list.push(id);
                } else {
                    return Err(String::from("Function parameters should be identifiers"));
                }
            }
        };
        while let Token::Comma = self.lexer.peek() {
            self.lexer.next();
            if let ASTNode::Variable(id) = self.item()? {
                list.push(id);
            } else {
                return Err(String::from("Function parameters should be identifiers"));
            }
        }
        Ok(list)
    }

    fn statement(&mut self) -> RASTNode {
        match self.lexer.peek() {
            Token::LeftBrace => self.block(),
            Token::If => self.if_stmt(),
            Token::While => self.while_stmt(),
            Token::For => self.for_stmt(),
            Token::Break => {
                self.lexer.next();
                self.expect(Token::Semicolon)?;
                Ok(ASTNode::Break)
            },
            Token::Continue => {
                self.lexer.next();
                self.expect(Token::Semicolon)?;
                Ok(ASTNode::Continue)
            },
            Token::Return => {
                self.lexer.next();
                let expr = match self.lexer.peek() {
                    Token::Semicolon => ASTNode::Null,
                    _ => self.expression()?,
                };
                self.expect(Token::Semicolon)?;
                Ok(ASTNode::Return(Box::new(expr)))
            },
            _ => self.expr_stmt(),
        }
    }

    fn if_stmt(&mut self) -> RASTNode {
        self.expect(Token::If)?;
        self.expect(Token::LeftParen)?;
        let expr = self.expression()?;
        self.expect(Token::RightParen)?;
        let stmt = self.statement()?;
        let else_stmt = if let Token::Else = self.lexer.peek() {
            self.lexer.next();
            Option::Some(self.statement()?)
        } else {
            Option::None
        };
        Ok(ASTNode::If(Box::new(expr), Box::new(stmt), Box::new(else_stmt)))
    }

    fn while_stmt(&mut self) -> RASTNode {
        self.expect(Token::While)?;
        self.expect(Token::LeftParen)?;
        let expr = self.expression()?;
        self.expect(Token::RightParen)?;
        Ok(ASTNode::While(Box::new(expr), Box::new(self.statement()?)))
    }

    fn for_stmt(&mut self) -> RASTNode {
        self.expect(Token::For)?;
        self.expect(Token::LeftParen)?;
        let init = match self.lexer.peek() {
            Token::Let => Option::Some(self.var_decl()?),
            Token::Semicolon => {
                self.lexer.next();
                Option::None
            },
            _ => Option::Some(self.expr_stmt()?),
        };
        let cond = match self.lexer.peek() {
            Token::Semicolon => ASTNode::Bool(true),
            _ => self.expression()?,
        };
        self.expect(Token::Semicolon)?;
        let incr = match self.lexer.peek() {
            Token::RightParen => Option::None,
            _ => Option::Some(self.expression()?),
        };
        self.expect(Token::RightParen)?;
        let mut res = self.statement()?;
        if let Option::Some(incr) = incr {
            res = ASTNode::Block(vec![res, ASTNode::ExprStmt(Box::new(incr))]);
        }
        res = ASTNode::While(Box::new(cond), Box::new(res));
        if let Option::Some(init) = init {
            res = ASTNode::Block(vec![init, res]);
        }
        Ok(res)
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
        Ok(ASTNode::Block(declarations))
    }

    fn expr_stmt(&mut self) -> RASTNode {
        let expr = self.expression()?;
        self.expect(Token::Semicolon)?;
        Ok(ASTNode::ExprStmt(Box::new(expr)))
    }

    fn expression(&mut self) -> RASTNode {
        self.assignment()
    }

    fn assignment(&mut self) -> RASTNode {
        let expr = self.logic_or()?;

        match self.lexer.peek() {
            Token::Equal | Token::PlusEqual | Token::DashEqual | Token::AsteriskEqual | Token::SlashEqual | Token::PercentEqual | Token::CaretEqual => {
                if let ASTNode::Variable(id) = expr.clone() {
                    let oper = self.lexer.next();
                    let mut value = self.assignment()?;
                    match oper {
                        Token::PlusEqual => value = ASTNode::Binary(Operator::Add, Box::new(expr), Box::new(value)),
                        Token::DashEqual => value = ASTNode::Binary(Operator::Subtract, Box::new(expr), Box::new(value)),
                        Token::AsteriskEqual => value = ASTNode::Binary(Operator::Multiply, Box::new(expr), Box::new(value)),
                        Token::SlashEqual => value = ASTNode::Binary(Operator::Divide, Box::new(expr), Box::new(value)),
                        Token::PercentEqual => value = ASTNode::Binary(Operator::Modulo, Box::new(expr), Box::new(value)),
                        Token::CaretEqual => value = ASTNode::Binary(Operator::Exponent, Box::new(expr), Box::new(value)),
                        _ => (),
                    };
                    Ok(ASTNode::Assign(id, Box::new(value)))
                } else {
                    Err(format!("Invalid assignment target {:?}", expr))
                }
            },
            _ => Ok(expr),
        }
    }

    fn logic_or(&mut self) -> RASTNode {
        let mut node = self.logic_and()?;
        while self.lexer.peek() == Token::Or {
            self.lexer.next();
            node = ASTNode::Binary(Operator::LogicOr, Box::new(node), Box::new(self.logic_and()?));
        }
        Ok(node)
    }

    fn logic_and(&mut self) -> RASTNode {
        let mut node = self.equality()?;
        while self.lexer.peek() == Token::And {
            self.lexer.next();
            node = ASTNode::Binary(Operator::LogicAnd, Box::new(node), Box::new(self.equality()?));
        }
        Ok(node)       
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
        let node = match self.lexer.peek() {
            Token::Not => {
                self.lexer.next();
                ASTNode::Unary(Operator::Not, Box::new(self.unary()?))
            },
            Token::Dash => {
                self.lexer.next();
                ASTNode::Unary(Operator::Subtract, Box::new(self.unary()?))
            },
            _ => self.call()?,
        };
        if let Token::Caret = self.lexer.peek() {
            self.lexer.next();
            Ok(ASTNode::Binary(Operator::Exponent, Box::new(node), Box::new(self.unary()?)))
        } else {
            Ok(node)
        }
    }

    fn call(&mut self) -> RASTNode {
        let mut node = self.item()?;
        if let Token::LeftParen = self.lexer.peek() {
            self.lexer.next();
            if let ASTNode::Variable(id) = node {
                node = ASTNode::Call(id, self.arguments()?)
            } else {
                return Err(format!("Invalid function name {:?}", node));
            }
            self.expect(Token::RightParen)?;
        }
        Ok(node)
    }

    fn arguments(&mut self) -> Result<Vec<ASTNode>, String> {
        let mut list = vec![];
        match self.lexer.peek() {
            Token::RightParen => (),
            _ => list.push(self.expression()?),
        };
        while let Token::Comma = self.lexer.peek() {
            self.lexer.next();
            list.push(self.expression()?);
        }
        Ok(list)
    }

    fn item(&mut self) -> RASTNode {
        let token = self.lexer.next();
        match token {
            Token::Number(x) => Ok(ASTNode::Number(x)),
            Token::Bool(x) => Ok(ASTNode::Bool(x)),
            Token::String(x) => Ok(ASTNode::String(x)),
            Token::Null => Ok(ASTNode::Null),
            Token::LeftParen => {
                let expr = self.expression()?;
                self.expect(Token::RightParen)?;
                Ok(expr)
            },
            Token::Identifier(x) => Ok(ASTNode::Variable(x)),
            _ => Err(format!("Invalid item token {:?}", token)),
        }
    }

    pub fn parse(&mut self) -> RASTNode {
        self.program()
    }

    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
}
