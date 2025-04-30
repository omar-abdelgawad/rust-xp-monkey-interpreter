use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::{self, BufRead, Write};

const PROMPT: &str = ">> ";

pub fn start(mut input: impl BufRead, mut output: impl Write) {
    let mut line = String::new();

    loop {
        // Print prompt
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();

        // Read user input
        line.clear();
        let bytes_read = input.read_line(&mut line).unwrap();
        if bytes_read == 0 {
            // EOF reached
            writeln!(output, "").unwrap();
            break;
        }

        // Create a lexer
        let mut lexer = Lexer::new(line.trim());

        // Tokenize input
        loop {
            let tok = lexer.next_token();
            if tok.ttype == TokenType::EOF {
                break;
            }
            writeln!(output, "{:?}", tok).unwrap();
        }
    }
}
