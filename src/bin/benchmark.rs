use std::time::Instant;

use clap::Parser;
use monkey_rs::ast::{Node, Program};
use monkey_rs::compiler::Compiler;
use monkey_rs::object::{ObjRef, ObjectTrait};
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

    let program = MonkeyParser::parse(input.to_string());

    println!("Finished parsing!");
    let engine: &mut dyn InterpreterEngine = match cli.engine {
        Engine::Vm => &mut VMEngine::new(),
        Engine::Eval => panic!("tree walking interpreter engine (mod evaluator) was removed"),
    };
    let (result, duration) = {
        println!("Started time!");
        let start = Instant::now();
        let result = engine.evaluate_program(program);
        let duration = start.elapsed();

        (result, duration)
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
trait InterpreterEngine {
    fn evaluate_program(&mut self, program: Program) -> ObjRef;
}
struct VMEngine {
    compiler: Compiler,
    //vm: VM,
}
impl VMEngine {
    fn new() -> Self {
        Self {
            compiler: Compiler::new(),
        }
    }
}
impl InterpreterEngine for VMEngine {
    fn evaluate_program(&mut self, program: Program) -> ObjRef {
        if let Err(err) = self.compiler.compile(Node::Program(program)) {
            panic!("compiler error: {}", err);
        }

        let bytecode = self.compiler.bytecode();
        let mut machine = VM::new(bytecode);

        if let Err(err) = machine.run() {
            panic!("vm error: {}", err);
        }

        machine.last_popped_stack_elem()
    }
}
