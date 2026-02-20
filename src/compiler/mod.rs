use crate::{ast, code::Instructions, object::Object};

#[derive(Debug)]
struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            instructions: Instructions::default(),
            constants: Default::default(),
        }
    }
}

impl Compiler {
    pub fn compile(&mut self, node: ast::Node) -> Result<(), ()> {
        todo!();
    }
    pub fn bytecode(&self) -> Bytecode {
        // should I clone here? I still am not sure
        Bytecode::new(self.instructions.clone(), self.constants.clone())
    }
}

/// The final output of the Compiler
#[derive(Debug)]
pub struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Bytecode {
    pub fn new(instructions: Instructions, constants: Vec<Object>) -> Self {
        Self {
            instructions,
            constants,
        }
    }
}

#[cfg(test)]
mod test {
    use std::any::Any;

    use crate::{
        code::{make, Opcode as Op},
        evaluator::test::test_integer_object,
        lexer::Lexer,
        parser::Parser,
    };

    use super::*;
    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<Box<dyn Any>>,
        expected_instructions: Vec<Instructions>,
    }
    impl CompilerTestCase {
        pub fn new(
            input: impl Into<String>,
            expected_constants: Vec<Box<dyn Any>>,
            expected_instructions: Vec<Instructions>,
        ) -> Self {
            Self {
                input: input.into(),
                expected_constants,
                expected_instructions,
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<CompilerTestCase> = vec![CompilerTestCase::new(
            "1 + 2",
            vec![Box::new(1), Box::new(2)],
            vec![
                Instructions::new(make(Op::Constant, &[0])),
                Instructions::new(make(Op::Constant, &[1])),
            ],
        )];
        run_compiler_tests(tests);
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for CompilerTestCase {
            input,
            expected_constants,
            expected_instructions,
        } in tests
        {
            let program = parse(input);
            let mut compiler = Compiler::default();
            compiler
                .compile(ast::Node::Program(program))
                .expect("compiler error");
            let bytecode = compiler.bytecode();

            test_instructions(expected_instructions, bytecode.instructions)
                .expect("test instructions failed");
            test_constants(expected_constants, bytecode.constants).expect("test constants failed");
        }
    }
    fn parse(input: String) -> ast::Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program()
    }
    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) -> Result<(), String> {
        let concatted = concat_instructions(expected);
        if actual.len() != concatted.len() {
            return Err(format!(
                "wrong instructions length.\n want={}, got={}",
                concatted.len(),
                actual.len()
            ));
        }
        for (i, ins) in concatted.iter().enumerate() {
            if actual[i] != *ins {
                return Err(format!(
                    "wrong instruction at {i}.\n want={:?}, got={:?}",
                    concatted, actual
                ));
            }
        }
        Ok(())
    }
    fn concat_instructions(s: Vec<Instructions>) -> Instructions {
        let ret: Vec<u8> = s.into_iter().flat_map(|instr| instr.0).collect();
        Instructions(ret)
    }
    fn test_constants(expected: Vec<Box<dyn Any>>, actual: Vec<Object>) -> Result<(), String> {
        if expected.len() != actual.len() {
            return Err(format!(
                "wrong number of constants. got={}, want={}",
                actual.len(),
                expected.len()
            ));
        }
        for (i, constant) in expected.iter().enumerate() {
            if let Some(exp_constant) = constant.downcast_ref::<i64>() {
                if !test_integer_object(&actual[i], *exp_constant) {
                    return Err(format!("constant {} - test_integer_object failed", i));
                }
            } else {
                return Err(format!("unknown dyn object"));
            }
        }
        Ok(())
    }
}
