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
}

pub fn tokenize(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();
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
                tokens.push(Token::Number(buffer.parse::<f64>().expect("Failed to parge to f64")));
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
                    "true" => tokens.push(Token::Bool(true)),
                    "false" => tokens.push(Token::Bool(false)),
                    _ => tokens.push(Token::Identifier(buffer)),
                }
            },
            '(' => tokens.push(Token::LeftParen),
            ')' => tokens.push(Token::RightParen),
            '=' => tokens.push(
                if *iter.peek().unwrap_or(&'\0') == '=' {
                    iter.next();
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            ),
            '!' => tokens.push(
                if *iter.peek().unwrap_or(&'\0') == '=' {
                    iter.next();
                    Token::NotEqual
                } else {
                    Token::Not
                }
            ),
            '>' => tokens.push(
                if *iter.peek().unwrap_or(&'\0') == '=' {
                    iter.next();
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            ),
            '<' => tokens.push(
                if *iter.peek().unwrap_or(&'\0') == '=' {
                    iter.next();
                    Token::LesserEqual
                } else {
                    Token::Lesser
                }
            ),
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Dash),
            '*' => tokens.push(Token::Asterisk),
            '/' => tokens.push(Token::Slash),
            '%' => tokens.push(Token::Percent),
            '^' => tokens.push(Token::Caret),
            ';' => tokens.push(Token::Semicolon),
            _ => (),
        }
    }

    return tokens;
}
