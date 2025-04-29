#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::all)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,  // add, foobar, x, y
    INT,    //123
    ASSIGN, // '='
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
#[allow(clippy::all)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
}
impl Token {
    pub fn new(ttype: TokenType, literal: impl Into<String>) -> Self {
        Token {
            ttype,
            literal: literal.into(),
        }
    }
}
