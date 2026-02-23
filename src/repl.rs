use crate::ast::Node;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::object::ObjectTrait;
use crate::parser::Parser;
use crate::vm::VM;
use std::cell::RefCell;
use std::io::{BufRead, Write};
use std::rc::Rc;

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
    let _env = Rc::new(RefCell::new(Environment::new()));

    loop {
        // Print prompt
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();

        //// Read user input
        line.clear();
        let bytes_read = input.read_line(&mut line).unwrap();
        if bytes_read == 0 {
            // EOF reached
            writeln!(output).unwrap();
            break;
        }
        //// End Read user input

        let l = Lexer::new(line.trim());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if !p.errors().is_empty() {
            print_parser_errors(&mut output, p.errors());
            continue;
        }
        //writeln!(output, "debug: {:#?}", program);
        writeln!(output, "prog exp: {}", program).unwrap();

        let mut comp = Compiler::new();
        if let Err(err) = comp.compile(Node::Program(program)) {
            writeln!(output, "Woops! Compilation failed:\n {err}\n");
            continue;
        }
        let mut machine = VM::new(comp.bytecode());
        if let Err(err) = machine.run() {
            writeln!(output, "Woops! Executing bytecode failed:\n {err}\n");
            continue;
        }
        let stacktop = machine
            .stack_top()
            .expect("stack must contain a value, right???");
        writeln!(output, "{}", stacktop.inspect()).unwrap();
    }
}
pub fn execute_file(mut input: impl BufRead, mut output: impl Write) {
    let mut file = String::new();
    input
        .read_to_string(&mut file)
        .expect("Failed to read whole file");
    let env = Rc::new(RefCell::new(Environment::new()));
    let l = Lexer::new(file);
    let mut p = Parser::new(l);
    let program = p.parse_program();
    if !p.errors().is_empty() {
        print_parser_errors(&mut output, p.errors());
        return;
    }
    use crate::evaluator;
    let evaluated = evaluator::eval(Node::Program(program), env.clone());
    writeln!(output, "{}", evaluated.inspect()).unwrap();
}

fn print_parser_errors(mut output: impl Write, errors: &[String]) {
    write!(output, "{}", MONKEY_FACE).unwrap();
    writeln!(output, "Woops! We ran into some monkey business here!").unwrap();
    writeln!(output, " parser errors:").unwrap();
    for msg in errors {
        writeln!(output, "\t{}", msg).unwrap();
    }
}
