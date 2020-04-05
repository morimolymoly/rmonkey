#![allow(dead_code)]

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    EOF,
    Ident(String),
    Int(u32),
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    Return,
    If,
    Else,
    Equal,
    NotEqual,
}

pub fn token_from_literal(literal: String) -> Token {
    match &*literal {
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        _ => Token::Ident(literal),
    }
}
