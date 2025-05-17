use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::TokenType;
use std::io::{BufRead, Write};

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
            writeln!(output).unwrap();
            break;
        }

        let l = Lexer::new(line.trim());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if !p.errors().is_empty() {
            print_parser_errors(&mut output, p.errors());
            continue;
        }
        writeln!(output, "{}", program);
    }
}
fn print_parser_errors(mut output: impl Write, errors: &[String]) {
    todo!();
}
