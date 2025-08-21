#[derive(Debug, PartialEq, Clone, Eq, Hash, Default)]
pub enum TokenType {
    ILLEGAL,
    #[default]
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
    //extended
    STRING,
    LBRACKET, // [ for arrays
    RBRACKET, // ] for arrays
    COLON,    // : for hash literals
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
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
