use std::collections::VecDeque;

#[derive(Debug)]
pub enum Token {
    Number(f64),
    Identifier(String),
    Bool(bool),

    LeftParen,
    RightParen,
    
    Equal,
    EqualEqual,
    Not,
    NotEqual,
    Greater,
    Lesser,
    GreaterEqual,
    LesserEqual,
    
    Plus,
    Dash,
    Asterisk,
    Slash,
    Percent,
    Caret,
    
    Semicolon,

    UNDEFINED,
}

#[derive(Debug)]
pub struct Lexer {
    tokens: VecDeque<Token>
}

impl Lexer {
    pub fn next(&mut self) -> Token {
        self.tokens.pop_back().unwrap_or(Token::UNDEFINED)
    }

    pub fn peek(&self) -> &Token {
        self.tokens.back().unwrap_or(&Token::UNDEFINED)
    }

    pub fn new(input: String) -> Lexer {
        let mut tokens = VecDeque::new();
        let mut iter = input.chars().peekable();

        while let Some(c) = iter.next() {
            match c {
                '0'..='9' => {
                    let mut buffer = String::new();
                    buffer += &c.to_string();
                    while let Some(d) = iter.peek() {
                        match d {
                            '0'..='9' => buffer += &iter.next().unwrap().to_string(),
                            _ => break,
                        }
                    }
                    tokens.push_back(Token::Number(buffer.parse::<f64>().expect("Failed to parge to f64")));
                },
                x if x.is_alphabetic() => {
                    let mut buffer = String::new();
                    buffer += &c.to_string();
                    while let Some(d) = iter.peek() {
                        match d {
                            y if y.is_alphabetic() => buffer += &iter.next().unwrap().to_string(),
                            _ => break,
                        }
                    }
                    match buffer.as_str() {
                        "true" => tokens.push_back(Token::Bool(true)),
                        "false" => tokens.push_back(Token::Bool(false)),
                        _ => tokens.push_back(Token::Identifier(buffer)),
                    }
                },
                '(' => tokens.push_back(Token::LeftParen),
                ')' => tokens.push_back(Token::RightParen),
                '=' => tokens.push_back(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::EqualEqual
                    } else {
                        Token::Equal
                    }
                ),
                '!' => tokens.push_back(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::NotEqual
                    } else {
                        Token::Not
                    }
                ),
                '>' => tokens.push_back(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::GreaterEqual
                    } else {
                        Token::Greater
                    }
                ),
                '<' => tokens.push_back(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::LesserEqual
                    } else {
                        Token::Lesser
                    }
                ),
                '+' => tokens.push_back(Token::Plus),
                '-' => tokens.push_back(Token::Dash),
                '*' => tokens.push_back(Token::Asterisk),
                '/' => tokens.push_back(Token::Slash),
                '%' => tokens.push_back(Token::Percent),
                '^' => tokens.push_back(Token::Caret),
                ';' => tokens.push_back(Token::Semicolon),
                _ => (),
            }
        }

        Lexer { tokens }
    }
}
