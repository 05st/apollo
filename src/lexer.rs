use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Identifier(String),
    Bool(bool),
    Str(String),
    Null,

    Let,
    Def,
    Print,
    If,
    Else,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    
    Equal,
    EqualEqual,
    Not,
    NotEqual,
    Greater,
    Lesser,
    GreaterEqual,
    LesserEqual,
    And,
    Or,
    
    Plus,
    Dash,
    Asterisk,
    Slash,
    Percent,
    Caret,
    
    Dot,
    Semicolon,

    UNDEFINED,
    EOF,
}

#[derive(Clone, Debug)]
pub struct Lexer {
    tokens: VecDeque<Token>
}

impl Lexer {
    pub fn next(&mut self) -> Token {
        self.tokens.pop_back().unwrap_or(Token::UNDEFINED)
    }

    pub fn peek(&self) -> Token {
        self.tokens.back().unwrap_or(&Token::UNDEFINED).clone()
    }

    pub fn new(input: String) -> Lexer {
        let mut tokens = VecDeque::new();
        let mut iter = input.trim().chars().peekable();

        while let Some(c) = iter.next() {
            match c {
                '0'..='9' => {
                    let mut buffer = String::new();
                    buffer += &c.to_string();
                    while let Some(d) = iter.peek() {
                        match d {
                            '0'..='9' | '.' => buffer += &iter.next().unwrap().to_string(),
                            _ => break,
                        }
                    }
                    tokens.push_front(Token::Number(buffer.parse::<f64>().expect("Failed to parse to f64")));
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut buffer = String::new();
                    buffer += &c.to_string();
                    while let Some(d) = iter.peek() {
                        match d {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => buffer += &iter.next().unwrap().to_string(),
                            _ => break,
                        }
                    }
                    match buffer.as_str() {
                        "true" => tokens.push_front(Token::Bool(true)),
                        "false" => tokens.push_front(Token::Bool(false)),
                        "null" => tokens.push_front(Token::Null),
                        "let" => tokens.push_front(Token::Let),
                        "def" => tokens.push_front(Token::Def),
                        "print" => tokens.push_front(Token::Print),
                        "if" => tokens.push_front(Token::If),
                        "else" => tokens.push_front(Token::Else),
                        _ => tokens.push_front(Token::Identifier(buffer)),
                    }
                },
                '"' => {
                    let mut buffer = String::new();
                    while let Some(d) = iter.peek() {
                        match d {
                            '"' => {
                                iter.next();
                                break;
                            },
                            _ => buffer += &iter.next().unwrap().to_string(),
                        }
                    }
                    tokens.push_front(Token::Str(buffer));
                },
                '(' => tokens.push_front(Token::LeftParen),
                ')' => tokens.push_front(Token::RightParen),
                '{' => tokens.push_front(Token::LeftBrace),
                '}' => tokens.push_front(Token::RightBrace),
                '[' => tokens.push_front(Token::LeftBracket),
                ']' => tokens.push_front(Token::RightBracket),
                '=' => tokens.push_front(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::EqualEqual
                    } else {
                        Token::Equal
                    }
                ),
                '!' => tokens.push_front(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::NotEqual
                    } else {
                        Token::Not
                    }
                ),
                '>' => tokens.push_front(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::GreaterEqual
                    } else {
                        Token::Greater
                    }
                ),
                '<' => tokens.push_front(
                    if *iter.peek().unwrap_or(&'\0') == '=' {
                        iter.next();
                        Token::LesserEqual
                    } else {
                        Token::Lesser
                    }
                ),
                '&' if *iter.peek().unwrap_or(&'\0') == '&' => {
                    iter.next();
                    tokens.push_front(Token::And)
                },
                '|' if *iter.peek().unwrap_or(&'\0') == '|' => {
                    iter.next();
                    tokens.push_front(Token::Or)
                },
                '+' => tokens.push_front(Token::Plus),
                '-' => tokens.push_front(Token::Dash),
                '*' => tokens.push_front(Token::Asterisk),
                '/' => tokens.push_front(Token::Slash),
                '%' => tokens.push_front(Token::Percent),
                '^' => tokens.push_front(Token::Caret),
                '.' => tokens.push_front(Token::Dot),
                ';' => tokens.push_front(Token::Semicolon),
                _ => (),
            }
        }

        tokens.push_front(Token::EOF);
        Lexer { tokens }
    }
}
