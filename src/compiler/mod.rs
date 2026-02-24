use crate::{
    ast,
    code::{make, Instructions, Opcode},
    object::Object,
};

#[derive(Debug)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    // FIX: these probably should be optional
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

#[derive(Debug, Clone)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}
impl EmittedInstruction {
    pub fn new(opcode: Opcode, position: usize) -> Self {
        Self { opcode, position }
    }
}
impl Default for EmittedInstruction {
    fn default() -> Self {
        Self {
            opcode: Opcode::Pop,
            position: Default::default(),
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Default::default(),
            constants: vec![],
            last_instruction: Default::default(),
            previous_instruction: Default::default(),
        }
    }

    pub fn compile(&mut self, node: ast::Node) -> Result<(), String> {
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
                St::Expression(exp_stmt) => {
                    self.compile(Node::Expression(*exp_stmt.expression))?;
                    self.emit(Opcode::Pop, &[]);
                }
                St::Let(let_statement) => todo!(),
                St::Return(return_statement) => todo!(),
                St::Block(block_stmt) => {
                    // I added the following if to avoid panicking on empty blocks
                    if block_stmt.statements.is_empty() {
                        self.emit(Opcode::Null, &[]);
                    }
                    for s in block_stmt.statements {
                        self.compile(Node::Statement(s))?;
                    }
                }
            },
            Node::Expression(exp) => match exp {
                Exp::Infix(infix_exp) => {
                    if infix_exp.operator == "<" {
                        // note that this means an expression is not guranteed to be evaluated in
                        // from left to right
                        self.compile(Node::Expression(*infix_exp.right))?;
                        self.compile(Node::Expression(*infix_exp.left))?;
                        self.emit(Opcode::GreaterThan, &[]);
                        return Ok(());
                    }
                    self.compile(Node::Expression(*infix_exp.left))?;
                    self.compile(Node::Expression(*infix_exp.right))?;
                    match infix_exp.operator.as_str() {
                        "+" => self.emit(Opcode::Add, &[]),
                        "-" => self.emit(Opcode::Sub, &[]),
                        "*" => self.emit(Opcode::Mul, &[]),
                        "/" => self.emit(Opcode::Div, &[]),
                        ">" => self.emit(Opcode::GreaterThan, &[]),
                        "==" => self.emit(Opcode::Equal, &[]),
                        "!=" => self.emit(Opcode::NotEqual, &[]),
                        _ => return Err(format!("unknown operator {}", infix_exp.operator)),
                    };
                }
                Exp::Identifier(identifier) => todo!(),
                Exp::Boolean(bool_lit) => {
                    match bool_lit.value {
                        true => self.emit(Opcode::True, &[]),
                        false => self.emit(Opcode::False, &[]),
                    };
                }
                Exp::Integer(int_lit) => {
                    let int_val = Object::new_int_var(int_lit.value);
                    let const_id = self.add_constant(int_val);
                    self.emit(Opcode::Constant, &[const_id as i64]);
                }
                Exp::Prefix(pre_exp) => {
                    self.compile(Node::Expression(*pre_exp.right))?;
                    match pre_exp.operator.as_str() {
                        "!" => self.emit(Opcode::Bang, &[]),
                        "-" => self.emit(Opcode::Minus, &[]),
                        _ => return Err(format!("unknown operator {}", pre_exp.operator)),
                    };
                }
                Exp::If(if_exp) => {
                    self.compile(Node::Expression(*if_exp.condition))?;
                    let jump_not_truthy_pos = self.emit(Opcode::JumpNotTruthy, &[9999]); // emit with bogus value
                    self.compile(Node::Statement(St::Block(*if_exp.consequence)))?;
                    // keep the last evaluated expression because if itself is an expression
                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }
                    let jump_pos = self.emit(Opcode::Jump, &[9999]);
                    let after_consequence_pos = self.instructions.len();
                    self.change_operand(
                        jump_not_truthy_pos,
                        after_consequence_pos.try_into().unwrap(),
                    );
                    match if_exp.alternative {
                        None => {
                            self.emit(Opcode::Null, &[]);
                        }
                        Some(alt_exp) => {
                            self.compile(Node::Statement(St::Block(*alt_exp)))?;
                            if self.last_instruction_is_pop() {
                                self.remove_last_pop();
                            }
                        }
                    };
                    let after_alternative_pos = self.instructions.len();
                    self.change_operand(jump_pos, after_alternative_pos.try_into().unwrap());
                }
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

    // writes an instruction and returns its pos
    fn emit(&mut self, op: Opcode, operands: &[i64]) -> usize {
        let ins = make(op, operands);
        let pos = self.add_instruction(&ins);
        self.set_last_instruction(op, pos);
        pos
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        std::mem::swap(&mut self.previous_instruction, &mut self.last_instruction);
        self.last_instruction = EmittedInstruction::new(op, pos);
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.0.extend(ins);
        pos_new_instruction
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.opcode == Opcode::Pop
    }

    fn remove_last_pop(&mut self) {
        self.instructions.0.truncate(self.last_instruction.position);
        self.last_instruction = self.previous_instruction.clone();
    }

    fn change_operand(&mut self, op_pos: usize, operand: i64) {
        let op: Opcode = TryFrom::try_from(self.instructions[op_pos]).unwrap();
        let new_instruction = make(op, &[operand]);

        self.replace_instruction(op_pos, &new_instruction);
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: &[u8]) {
        let end = pos + new_instruction.len();
        self.instructions.0[pos..end].copy_from_slice(new_instruction);
    }
}

/// The final output of the Compiler
#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
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
mod tests {
    use std::any::Any;

    use crate::{
        code::{make, Opcode as Op},
        evaluator::tests::test_integer_object,
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
        let tests: Vec<CompilerTestCase> = vec![
            CompilerTestCase::new(
                "1 + 2",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Add, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "1; 2",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Pop, &[])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "1 - 2",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Sub, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "1 * 2",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Mul, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "2 / 1",
                vec![Box::new(2i64), Box::new(1i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Div, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "-1",
                vec![Box::new(1i64)], // the constant is 1
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Minus, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase::new(
                "true",
                vec![],
                vec![
                    Instructions::new(make(Op::True, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "false",
                vec![],
                vec![
                    Instructions::new(make(Op::False, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "1 > 2",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::GreaterThan, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "1 < 2",
                vec![Box::new(2i64), Box::new(1i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::GreaterThan, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "1 == 2",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Equal, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "1 != 2",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::NotEqual, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "true == false",
                vec![],
                vec![
                    Instructions::new(make(Op::True, &[])),
                    Instructions::new(make(Op::False, &[])),
                    Instructions::new(make(Op::Equal, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "true != false",
                vec![],
                vec![
                    Instructions::new(make(Op::True, &[])),
                    Instructions::new(make(Op::False, &[])),
                    Instructions::new(make(Op::NotEqual, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "!true",
                vec![],
                vec![
                    Instructions::new(make(Op::True, &[])),
                    Instructions::new(make(Op::Bang, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
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
            let mut compiler = Compiler::new();
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
                "wrong instructions length.\n want={:?}\n got ={:?}",
                concatted.to_string(),
                actual.to_string()
            ));
        }
        for (i, ins) in concatted.iter().enumerate() {
            if actual[i] != *ins {
                return Err(format!(
                    "wrong instruction at {i}.\n want={:?}\n got ={:?}",
                    concatted.to_string(),
                    actual.to_string()
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
    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase::new(
                "if (true) { 10 }; 3333;",
                vec![Box::new(10i64), Box::new(3333i64)],
                vec![
                    // 0000 1 byte
                    Instructions::new(make(Op::True, &[])),
                    // 0001 3 bytes
                    Instructions::new(make(Op::JumpNotTruthy, &[10])),
                    // 0004 3 bytes
                    Instructions::new(make(Op::Constant, &[0])),
                    // 0007 3 bytes
                    Instructions::new(make(Op::Jump, &[11])),
                    // 0010 1 byte
                    Instructions::new(make(Op::Null, &[])),
                    // 0011 1 byte
                    Instructions::new(make(Op::Pop, &[])),
                    // 0012 3 bytes
                    Instructions::new(make(Op::Constant, &[1])),
                    // 0015 1 byte
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "if (true) { 10 } else { 20 }; 3333;",
                vec![Box::new(10i64), Box::new(20i64), Box::new(3333i64)],
                vec![
                    // 0000 1 byte
                    Instructions::new(make(Op::True, &[])),
                    // 0001 3 bytes
                    Instructions::new(make(Op::JumpNotTruthy, &[10])),
                    // 0004 3 bytes
                    Instructions::new(make(Op::Constant, &[0])),
                    // 0007 3 bytes
                    Instructions::new(make(Op::Jump, &[13])),
                    // 0010 3 bytes
                    Instructions::new(make(Op::Constant, &[1])),
                    // 0013 1 byte
                    Instructions::new(make(Op::Pop, &[])),
                    // 0014 3 bytes
                    Instructions::new(make(Op::Constant, &[2])),
                    // 0017 1 byte
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_custom_empty_block_returns_null() {
        let tests = vec![
            CompilerTestCase::new(
                "if (false) { 10 } else { };",
                vec![Box::new(10i64)],
                vec![
                    // 0000 1 byte
                    Instructions::new(make(Op::False, &[])),
                    // 0001 3 bytes
                    Instructions::new(make(Op::JumpNotTruthy, &[10])),
                    // 0004 3 bytes
                    Instructions::new(make(Op::Constant, &[0])),
                    // 0007 3 bytes
                    Instructions::new(make(Op::Jump, &[11])),
                    // 0010 1 bytes
                    Instructions::new(make(Op::Null, &[])),
                    // 0011 1 byte
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "if (false) {}",
                vec![],
                vec![
                    // 0000 1 byte
                    Instructions::new(make(Op::False, &[])),
                    // 0001 3 bytes
                    Instructions::new(make(Op::JumpNotTruthy, &[8])),
                    // 0004 1 bytes
                    Instructions::new(make(Op::Null, &[])),
                    // 0005 3 bytes
                    Instructions::new(make(Op::Jump, &[9])),
                    // 0008 1 bytes
                    Instructions::new(make(Op::Null, &[])),
                    // 0009 1 byte
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
}
