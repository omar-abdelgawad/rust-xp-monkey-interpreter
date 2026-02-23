use crate::ast;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

// TODO: remove this copied function
fn parse(input: String) -> ast::Program {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    p.parse_program()
}

// TODO: remove one of duplicated function
pub fn test_integer_object(expected: i64, actual: &Object) -> Result<(), String> {
    if let Object::Integer(result) = actual {
        if result.value != expected {
            Err(format!(
                "object has wrong value. got={}, want{}",
                result.value, expected
            ))
        } else {
            Ok(())
        }
    } else {
        Err(format!("object is not Integer. got=({:?})", actual))
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::compiler::{self, Compiler};

    use super::*;
    #[derive(Debug)]
    struct VmTestCase {
        input: String,
        expected: Box<dyn Any>,
    }

    impl VmTestCase {
        fn new(input: impl Into<String>, expected: impl Any + 'static) -> Self {
            Self {
                input: input.into(),
                expected: Box::new(expected),
            }
        }
    }
    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase::new("1", 1i64),
            VmTestCase::new("2", 2i64),
            VmTestCase::new("1 + 2", 2i64), // FIXME
        ];
        run_vm_tests(tests);
    }
    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for VmTestCase { input, expected } in tests {
            let program = parse(input);
            let mut comp = Compiler::new();
            comp.compile(ast::Node::Program(program))
                .unwrap_or_else(|e| panic!("compiler error: {e:?}"));
            todo!();
            //let mut vm = VM::new();
            //vm.run().unwrap_or_else(|e| panic!("vm error: {e}"));
            //let stack_elm = vm.stack_top();
            //test_expected_object(expected, stack_elm);
        }
    }

    fn test_expected_object(expected: &dyn Any, actual: &Object) {
        if let Some(expec) = expected.downcast_ref::<i64>() {
            test_integer_object(*expec, actual)
                .unwrap_or_else(|e| panic!("test_integer_object failed: {e}"))
        } else {
            panic!("unknown dyn object: {:?}", expected);
        }
    }
}
