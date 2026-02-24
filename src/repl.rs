use crate::ast::Node;
use crate::compiler::symbol_table::SymbolTable;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::object::{ObjectTrait, GARBAGEVALOBJ};
use crate::parser::Parser;
use crate::vm::{GLOBALSSIZE, VM};
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

    // TODO: in the book he uses pointers to avoid mutating them manually inside the loop but I am
    // too lazy to thing about this now. I don't like this api anyways and I should change it in
    // the future and add a test case as well.
    let mut constants = vec![];
    let mut globals = vec![GARBAGEVALOBJ; GLOBALSSIZE];
    let mut symbol_table = SymbolTable::new();

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

        let mut comp = Compiler::new_with_state(symbol_table.clone(), constants.clone());
        if let Err(err) = comp.compile(Node::Program(program)) {
            writeln!(output, "Woops! Compilation failed:\n {err}\n");
            continue;
        }
        let code = comp.bytecode();
        constants = code.constants.clone();
        symbol_table = comp.symbol_table();

        let mut machine = VM::new_with_globals_store(code, globals.clone());
        if let Err(err) = machine.run() {
            writeln!(output, "Woops! Executing bytecode failed:\n {err}\n");
            continue;
        }
        globals = machine.globals();

        let last_popped = machine.last_popped_stack_elem();
        writeln!(output, "{}", last_popped.inspect()).unwrap();
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
