use crate::ast::Node;
use crate::object::ObjectTrait;
use crate::parser::Parser;
use crate::token::TokenType;
use crate::{evaluator, lexer::Lexer};
use std::io::{BufRead, Write};

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"`` ``"-./, -' /
   `'-' /_   ^ ^   _\ '-'`
       |  \._   _./  |
       \   \ `~` /   /
        '._ '-=-' _.'
           '~---~'
"#;

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
        writeln!(output, "debug:{:#?}", program);
        writeln!(output, "prog:{}", program);
        let evaluated = evaluator::eval(Node::Program(program));
        if evaluated.is_some() {
            writeln!(output, "eval:{}", evaluated.unwrap().inspect());
        }
    }
}
fn print_parser_errors(mut output: impl Write, errors: &[String]) {
    write!(output, "{}", MONKEY_FACE);
    write!(output, "Woops! We ran into some monkey business here!\n");
    write!(output, " parser errors:\n");
    for msg in errors {
        write!(output, "\t{}\n", msg);
    }
}
