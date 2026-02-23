use crate::{
    ast,
    code::{make, Instructions, Opcode},
    object::Object,
};

#[derive(Debug, Default)]
struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn compile(&mut self, node: ast::Node) -> Result<(), ()> {
        use crate::ast::Expression as Exp;
        use crate::ast::Node;
        use crate::ast::Statement as St;
        match node {
            Node::Program(program) => {
                for s in program.statements {
                    self.compile(Node::Statement(s))?
                }
            }
            Node::Statement(stmt) => match stmt {
                St::Expression(exp_stmt) => self.compile(Node::Expression(*exp_stmt.expression))?,
                St::Let(let_statement) => todo!(),
                St::Return(return_statement) => todo!(),
                St::Block(block_statement) => todo!(),
            },
            Node::Expression(exp) => match exp {
                Exp::Infix(infix_exp) => {
                    self.compile(Node::Expression(*infix_exp.left))?;
                    self.compile(Node::Expression(*infix_exp.right))?
                }
                Exp::Identifier(identifier) => todo!(),
                Exp::Boolean(boolean) => todo!(),
                Exp::Integer(int_lit) => {
                    let int_val = Object::new_int_var(int_lit.value);
                    let const_id = self.add_constant(int_val);
                    self.emit(Opcode::Constant, &[const_id as i64]);
                }
                Exp::Prefix(prefix_expression) => todo!(),
                Exp::If(if_expression) => todo!(),
                Exp::Function(function_literal) => todo!(),
                Exp::Call(call_expression) => todo!(),
                Exp::Str(string_literal) => todo!(),
                Exp::Arr(array_literal) => todo!(),
                Exp::Ind(index_expression) => todo!(),
                Exp::Hash(hash_literal) => todo!(),
                Exp::While(while_expression) => todo!(),
            },
        }
        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        // should I clone here? I still am not sure
        Bytecode::new(self.instructions.clone(), self.constants.clone())
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: Opcode, operands: &[i64]) -> usize {
        let ins = make(op, operands);
        self.add_instruction(&ins)
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.0.extend(ins);
        pos_new_instruction
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
            vec![Box::new(1i64), Box::new(2i64)],
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
                .unwrap_or_else(|e| panic!("test instructions failed: {e}"));
            test_constants(expected_constants, bytecode.constants)
                .unwrap_or_else(|e| panic!("test constants failed: {e}"));
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
                "wrong instructions length.\n want={}\n got={}",
                concatted, actual
            ));
        }
        for (i, ins) in concatted.iter().enumerate() {
            if actual[i] != *ins {
                return Err(format!(
                    "wrong instruction at {i}.\n want={:?}\n got={:?}",
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
                return Err(format!("unknown dyn object: {:?}", constant));
            }
        }
        Ok(())
    }
}
