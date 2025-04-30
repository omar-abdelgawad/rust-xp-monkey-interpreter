#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::all)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT, // add, foobar, x, y
    INT,   //123
    // Operators
    ASSIGN, // '='
    PLUS,
    MINUS,
    BANG,
    ASTERISK, // *
    SLASH,    // /

    LT,     // <
    GT,     // >
    EQ,     // ==
    NOT_EQ, // !=

    COMMA,
    SEMICOLON,
    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    //keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}
#[allow(clippy::all)]
#[derive(Debug)]
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
impl Default for Token {
    fn default() -> Self {
        Token {
            ttype: TokenType::EOF,
            literal: "".to_string(),
        }
    }
}
