use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Identifier(String),
    Bool(bool),
    String(String),
    Null,

    Let,
    Def,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Return,
    Obj,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    
    Equal,
    PlusEqual,
    DashEqual,
    AsteriskEqual,
    SlashEqual,
    PercentEqual,
    CaretEqual,

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
    Comma,
    Colon,

    UNDEFINED,
    EOF,
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
        let mut iter = input.trim().chars().peekable();

        while let Some(c) = iter.next() {
            let peek = *iter.peek().unwrap_or(&'\0');
            let mut double_match = |a, b, x, y| {
                if a == b {
                    iter.next();
                    x
                } else {
                    y
                }
            };

            match c {
                '0'..='9' => {
                    let mut buffer = c.to_string();
                    while let Some(d) = iter.peek() {
                        match d {
                            '0'..='9' | '.' => buffer += &iter.next().unwrap().to_string(),
                            _ => break,
                        }
                    }
                    tokens.push_front(Token::Number(buffer.parse::<f64>().expect("Failed to parse to f64")));
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut buffer = c.to_string();
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
                        "if" => tokens.push_front(Token::If),
                        "else" => tokens.push_front(Token::Else),
                        "while" => tokens.push_front(Token::While),
                        "for" => tokens.push_front(Token::For),
                        "break" => tokens.push_front(Token::Break),
                        "continue" => tokens.push_front(Token::Continue),
                        "return" => tokens.push_front(Token::Return),
                        "obj" => tokens.push_front(Token::Obj),
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
                    tokens.push_front(Token::String(buffer));
                },
                '(' => tokens.push_front(Token::LeftParen),
                ')' => tokens.push_front(Token::RightParen),
                '{' => tokens.push_front(Token::LeftBrace),
                '}' => tokens.push_front(Token::RightBrace),
                '[' => tokens.push_front(Token::LeftBracket),
                ']' => tokens.push_front(Token::RightBracket),
                '=' => tokens.push_front(double_match(peek, '=', Token::EqualEqual, Token::Equal)),
                '!' => tokens.push_front(double_match(peek, '=', Token::NotEqual, Token::Not)),
                '>' => tokens.push_front(double_match(peek, '=', Token::GreaterEqual, Token::Greater)),
                '<' => tokens.push_front(double_match(peek, '=', Token::LesserEqual, Token::Lesser)),
                '+' => tokens.push_front(double_match(peek, '=', Token::PlusEqual, Token::Plus)),
                '-' => tokens.push_front(double_match(peek, '=', Token::DashEqual, Token::Dash)),
                '*' => tokens.push_front(double_match(peek, '=', Token::AsteriskEqual, Token::Asterisk)),
                '/' => {
                    if peek == '/' {
                        while let Some(d) = iter.next() {
                            if d == '\n' {
                                break
                            }
                        }
                    } else if peek == '=' {
                        iter.next();
                        tokens.push_front(Token::SlashEqual);
                    } else {
                        tokens.push_front(Token::Slash);
                    }
                },
                '%' => tokens.push_front(double_match(peek, '=', Token::PercentEqual, Token::Percent)),
                '^' => tokens.push_front(double_match(peek, '=', Token::CaretEqual, Token::Caret)),
                '&' if *iter.peek().unwrap_or(&'\0') == '&' => {
                    iter.next();
                    tokens.push_front(Token::And)
                },
                '|' if *iter.peek().unwrap_or(&'\0') == '|' => {
                    iter.next();
                    tokens.push_front(Token::Or)
                },
                '.' => tokens.push_front(Token::Dot),
                ';' => tokens.push_front(Token::Semicolon),
                ',' => tokens.push_front(Token::Comma),
                ':' => tokens.push_front(Token::Colon),
                _ => (),
            }
        }

        tokens.push_front(Token::EOF);
        Lexer { tokens }
    }
}
