pub type TokenType = &'static str;

pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

// 識別子 + リテラル
pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";

// 演算子
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";

//デリミタ
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";

// Keyword
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";


pub enum Token2 {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
}

pub fn token_from_literal(literal: &str) -> TokenType {
    match literal {
        "let" => LET,
        "fn" => FUNCTION,
        _ => IDENT,
    }
}
