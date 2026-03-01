use std::cell::RefCell;
use std::rc::Rc;
use std::time::Instant;

use clap::Parser;
use monkey_rs::ast::Node;
use monkey_rs::compiler::Compiler;
use monkey_rs::evaluator::eval;
use monkey_rs::lexer::Lexer;
use monkey_rs::object::environment::Environment;
use monkey_rs::object::ObjectTrait;
use monkey_rs::parser::Parser as MonkeyParser;
use monkey_rs::vm::VM;

fn main() {
    let cli = Cli::parse();
    let input = r#"
    let fibonacci = fn(x) {
        if (x == 0) {
            0
        } else {
            if (x == 1) {
                return 1;
            } else {
                fibonacci(x - 1) + fibonacci(x - 2);
            }
        }
    };
    fibonacci(35);
    "#;

    let l = Lexer::new(input.to_string());
    let mut p = MonkeyParser::new(l);
    let program = p.parse_program();

    println!("Finished parsing!");
    let (result, duration) = match cli.engine {
        Engine::Vm => {
            let mut compiler = Compiler::new();
            if let Err(err) = compiler.compile(Node::Program(program)) {
                eprintln!("compiler error: {}", err);
                return;
            }

            let bytecode = compiler.bytecode();
            let mut machine = VM::new(bytecode);

            println!("Started time after compiling!");
            let start = Instant::now();
            if let Err(err) = machine.run() {
                eprintln!("vm error: {}", err);
                return;
            }
            let duration = start.elapsed();

            let result = machine.last_popped_stack_elem();
            (result, duration)
        }

        Engine::Eval => {
            let env = Rc::new(RefCell::new(Environment::new()));

            println!("Started time!");
            let start = Instant::now();
            let result = eval(Node::Program(program), env);
            let duration = start.elapsed();

            (result, duration)
        }
    };

    println!(
        "engine={:?}, result={}, duration={duration:?}",
        cli.engine,
        result.inspect(),
    );
}

#[derive(clap::Parser)]
#[command(version, about)]
struct Cli {
    /// Execution engine to use
    #[arg(long)]
    engine: Engine,
}

#[derive(Debug, Clone, PartialEq, clap::ValueEnum)]
enum Engine {
    Vm,
    Eval,
}
