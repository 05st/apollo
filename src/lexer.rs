use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub enum Token {
    Number(f64),
    Identifier(String),
    Bool(bool),

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
    
    Plus,
    Dash,
    Asterisk,
    Slash,
    Percent,
    Caret,
    
    Dot,
    Semicolon,

    UNDEFINED,
}

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
        let split = input.split_whitespace().collect::<String>();
        let mut iter = split.trim().chars().peekable();

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
                    tokens.push_front(Token::Number(buffer.parse::<f64>().expect("Failed to parge to f64")));
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
                        _ => tokens.push_front(Token::Identifier(buffer)),
                    }
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

        Lexer { tokens }
    }
}
