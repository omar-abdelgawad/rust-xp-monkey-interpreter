use monkey_rs::ast::Node;
use monkey_rs::compiler::Compiler;
use monkey_rs::object::ObjectTrait;
use monkey_rs::parser::Parser as MonkeyParser;
use monkey_rs::vm::VM;

fn main() {
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

    let (result, duration) = {
        println!("Started time!");
        let start = std::time::Instant::now();
        
        let mut compiler = Compiler::new();
        if let Err(err) = compiler.compile(Node::Program(program)) {
            panic!("compiler error: {}", err);
        }

        let bytecode = compiler.bytecode();
        let mut machine = VM::new(bytecode);

        if let Err(err) = machine.run() {
            panic!("vm error: {}", err);
        }

        let result = machine.last_popped_stack_elem();
        let duration = start.elapsed();

        (result, duration)
    };

    println!(
        "engine=Vm, result={}, duration={duration:?}",
        result.inspect(),
    );
}


