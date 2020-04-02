#![allow(dead_code)]

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    Ident(String),
    Int(u32),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
}

pub fn token_from_literal(literal: String) -> Token {
    match &*literal {
        "let" => Token::Let,
        "fn" => Token::Function,
        _ => Token::Ident(literal),
    }
}
