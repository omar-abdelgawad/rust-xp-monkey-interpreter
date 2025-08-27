use std::collections::HashMap;

use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: i32,
    readPosition: i32,
    ch: u8,
    keywords: HashMap<String, TokenType>,
}
impl Lexer {
    pub fn new(input: impl Into<String>) -> Self {
        let mut keywords = Self::get_keywords_hashmap();
        let mut l = Lexer {
            input: input.into(),
            position: 0,
            readPosition: 0,
            ch: 0,
            keywords,
        };
        l.read_char();
        l
    }
    fn get_keywords_hashmap() -> HashMap<String, TokenType> {
        let mut keywords = HashMap::new();
        keywords.insert("fn".to_string(), TokenType::FUNCTION);
        keywords.insert("let".to_string(), TokenType::LET);
        keywords.insert("true".to_string(), TokenType::TRUE);
        keywords.insert("false".to_string(), TokenType::FALSE);
        keywords.insert("if".to_string(), TokenType::IF);
        keywords.insert("else".to_string(), TokenType::ELSE);
        keywords.insert("return".to_string(), TokenType::RETURN);
        keywords.insert("while".to_string(), TokenType::WHILE);
        keywords
    }
    fn skip_white_space(&mut self) {
        while self.ch.is_ascii_whitespace() || self.ch == b'\t' || self.ch == b'\r' {
            self.read_char();
        }
    }
    fn read_char(&mut self) {
        if self.readPosition >= self.input.len() as i32 {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.readPosition as usize];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }
    fn peak_char(&self) -> u8 {
        if self.readPosition as usize >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.readPosition as usize]
        }
    }
    fn lookup_ident(&self, ident: &str) -> TokenType {
        if let Some(tok) = self.keywords.get(ident) {
            tok.clone()
        } else {
            TokenType::IDENT
        }
    }
    fn read_number(&mut self) -> Token {
        let position = self.position as usize;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        let ident_slice = &self.input[position..self.position as usize];
        Token::new(TokenType::INT, ident_slice)
    }
    fn read_identifier(&mut self) -> Token {
        let position = self.position as usize;
        while is_letter(self.ch) {
            self.read_char();
        }
        let ident_slice = &self.input[position..self.position as usize];
        let tok_type = self.lookup_ident(ident_slice);
        Token::new(tok_type, ident_slice)
    }
    fn read_string(&mut self) -> String {
        let position: usize = (self.position + 1) as usize;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }
        self.input[position..self.position as usize].to_owned()
    }
    /// gets next_token;#![wa()]
    pub fn next_token(&mut self) -> Token {
        self.skip_white_space();
        let out = match self.ch {
            b'=' => {
                if self.peak_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    Token::new(TokenType::EQ, format!("{}{}", ch as char, self.ch as char))
                } else {
                    Token::new(TokenType::ASSIGN, self.ch as char)
                }
            }
            b'+' => Token::new(TokenType::PLUS, self.ch as char),
            b'-' => Token::new(TokenType::MINUS, self.ch as char),
            b'!' => {
                if self.peak_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    Token::new(
                        TokenType::NOT_EQ,
                        format!("{}{}", ch as char, self.ch as char),
                    )
                } else {
                    Token::new(TokenType::BANG, self.ch as char)
                }
            }
            b'/' => Token::new(TokenType::SLASH, self.ch as char),
            b'*' => Token::new(TokenType::ASTERISK, self.ch as char),
            b'<' => Token::new(TokenType::LT, self.ch as char),
            b'>' => Token::new(TokenType::GT, self.ch as char),
            b';' => Token::new(TokenType::SEMICOLON, self.ch as char),
            b'(' => Token::new(TokenType::LPAREN, self.ch as char),
            b')' => Token::new(TokenType::RPAREN, self.ch as char),
            b',' => Token::new(TokenType::COMMA, self.ch as char),
            b'{' => Token::new(TokenType::LBRACE, self.ch as char),
            b'}' => Token::new(TokenType::RBRACE, self.ch as char),
            b'"' => Token::new(TokenType::STRING, self.read_string()),
            b'[' => Token::new(TokenType::LBRACKET, self.ch as char),
            b']' => Token::new(TokenType::RBRACKET, self.ch as char),
            b':' => Token::new(TokenType::COLON, self.ch as char),
            0 => Token::new(TokenType::EOF, ""),
            _ => {
                if is_letter(self.ch) {
                    return self.read_identifier();
                } else if self.ch.is_ascii_digit() {
                    return self.read_number();
                } else {
                    Token::new(TokenType::ILLEGAL, self.ch as char)
                }
            }
        };
        self.read_char();
        out
    }
}
fn is_letter(ch: u8) -> bool {
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == b'_'
}
#[cfg(test)]
mod tests {
    use super::*; // <- cleaner, imports Token and TokenType
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        const INPUT: &str = "=+(){},;";
        // Define the expected sequence of tokens
        let tests = [
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::PLUS, "+"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(INPUT); // assuming you have a Lexer struct with new()

        for (i, expected) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(
                tok.ttype, expected.ttype,
                "tests[{}] - token_type wrong. expected={:?}, got={:?}",
                i, expected.ttype, tok.ttype
            );
            assert_eq!(
                tok.literal, expected.literal,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, expected.literal, tok.literal
            );
        }
    }
    #[test]
    fn test_monkey_src_lexer() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
{\"foo\": \"bar\"}
while
";
        let tests = [
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "five"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "ten"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "add"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::FUNCTION, "fn"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::IDENT, "x"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::IDENT, "y"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::IDENT, "x"),
            Token::new(TokenType::PLUS, "+"),
            Token::new(TokenType::IDENT, "y"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LET, "let"),
            Token::new(TokenType::IDENT, "result"),
            Token::new(TokenType::ASSIGN, "="),
            Token::new(TokenType::IDENT, "add"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::IDENT, "five"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::IDENT, "ten"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::BANG, "!"),
            Token::new(TokenType::MINUS, "-"),
            Token::new(TokenType::SLASH, "/"),
            Token::new(TokenType::ASTERISK, "*"),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::LT, "<"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::GT, ">"),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::IF, "if"),
            Token::new(TokenType::LPAREN, "("),
            Token::new(TokenType::INT, "5"),
            Token::new(TokenType::LT, "<"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::RPAREN, ")"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::RETURN, "return"),
            Token::new(TokenType::TRUE, "true"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::ELSE, "else"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::RETURN, "return"),
            Token::new(TokenType::FALSE, "false"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::EQ, "=="),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::INT, "10"),
            Token::new(TokenType::NOT_EQ, "!="),
            Token::new(TokenType::INT, "9"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::STRING, "foobar"),
            Token::new(TokenType::STRING, "foo bar"),
            Token::new(TokenType::LBRACKET, "["),
            Token::new(TokenType::INT, "1"),
            Token::new(TokenType::COMMA, ","),
            Token::new(TokenType::INT, "2"),
            Token::new(TokenType::RBRACKET, "]"),
            Token::new(TokenType::SEMICOLON, ";"),
            Token::new(TokenType::LBRACE, "{"),
            Token::new(TokenType::STRING, "foo"),
            Token::new(TokenType::COLON, ":"),
            Token::new(TokenType::STRING, "bar"),
            Token::new(TokenType::RBRACE, "}"),
            Token::new(TokenType::WHILE, "while"),
            Token::new(TokenType::EOF, ""),
        ];
        let mut lexer = Lexer::new(input);

        for (i, expected) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(
                tok.literal, expected.literal,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, expected.literal, tok.literal
            );
            assert_eq!(
                tok.ttype, expected.ttype,
                "tests[{}] - token_type wrong. expected={:?}, got={:?}",
                i, expected.ttype, tok.ttype
            );
        }
    }
}
