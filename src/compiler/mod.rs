use crate::{
    ast::{self, LetStatement},
    code::{make, Instructions, Opcode},
    compiler::symbol_table::SymbolTable,
    object::{CompiledFunctionObj, Object},
};
pub mod symbol_table;

#[derive(Debug)]
pub struct Compiler {
    constants: Vec<Object>,
    symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

#[derive(Debug, Clone, Default)]
struct CompilationScope {
    instructions: Instructions,
    // FIX: these probably should be optional
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}
impl CompilationScope {
    fn new(
        instructions: Instructions,
        last_instruction: EmittedInstruction,
        previous_instruction: EmittedInstruction,
    ) -> Self {
        Self {
            instructions,
            last_instruction,
            previous_instruction,
        }
    }
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
    // should remove this function but need it for now
    pub fn symbol_table(&self) -> SymbolTable {
        self.symbol_table.clone()
    }
    pub fn new() -> Self {
        let main_scope = CompilationScope::default();
        Self {
            constants: vec![],
            symbol_table: SymbolTable::new(),
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }
    fn current_instructions(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
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
                St::Let(let_stmt) => {
                    let LetStatement { name, value, .. } = let_stmt;
                    self.compile(Node::Expression(*value))?;
                    let symbol = self.symbol_table.define(name.value);
                    self.emit(Opcode::SetGlobal, &[symbol.index as i64]);
                }
                St::Return(ret_stmt) => {
                    self.compile(Node::Expression(*ret_stmt.return_value));
                    self.emit(Opcode::ReturnValue, &[]);
                }
                St::Block(block_stmt) => {
                    //// I added the following if to avoid panicking on empty blocks
                    //if block_stmt.statements.is_empty() {
                    //    self.emit(Opcode::Null, &[]);
                    //}
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
                Exp::Identifier(ident) => {
                    let symbol = self
                        .symbol_table
                        .resolve(&ident.value)
                        .ok_or(format!("undefined variable {}", ident.value))?;
                    self.emit(Opcode::GetGlobal, &[symbol.index as i64]);
                }
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
                    if self.last_instruction_is(Opcode::Pop) {
                        self.remove_last_pop();
                    }
                    let jump_pos = self.emit(Opcode::Jump, &[9999]);
                    let after_consequence_pos = self.current_instructions().len();
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
                            if self.last_instruction_is(Opcode::Pop) {
                                self.remove_last_pop();
                            }
                        }
                    };
                    let after_alternative_pos = self.current_instructions().len();
                    self.change_operand(jump_pos, after_alternative_pos.try_into().unwrap());
                }
                Exp::Function(fn_lit) => {
                    self.enter_scope();
                    let fn_lit_body = fn_lit.body.unwrap(); //FIX: what to do here?
                    self.compile(Node::Statement(St::Block(*fn_lit_body)))?;
                    if self.last_instruction_is(Opcode::Pop) {
                        self.replace_last_pop_with_return();
                    }
                    if !self.last_instruction_is(Opcode::ReturnValue) {
                        self.emit(Opcode::Return, &[]);
                    }
                    let instructions = self.leave_scope();
                    let compiled_fn = CompiledFunctionObj::new(instructions);
                    let const_ind = self.add_constant(Object::CompiledFunction(compiled_fn));
                    self.emit(Opcode::Constant, &[const_ind as i64]);
                }
                Exp::Call(call_expression) => todo!(),
                Exp::Str(str_lit) => {
                    let str_obj = Object::new_str_var(&str_lit.value);
                    let const_id = self.add_constant(str_obj);
                    self.emit(Opcode::Constant, &[const_id as i64]);
                }
                Exp::Arr(arr_lit) => {
                    let arr_len = arr_lit.elements.len();
                    for el in arr_lit.elements {
                        self.compile(Node::Expression(el))?;
                    }
                    self.emit(Opcode::Array, &[arr_len as i64]);
                }
                Exp::Ind(ind_exp) => {
                    self.compile(Node::Expression(*ind_exp.left))?;
                    self.compile(Node::Expression(*ind_exp.index))?;
                    self.emit(Opcode::Index, &[]);
                }
                Exp::Hash(hash_lit) => {
                    // AI generated cause I was too lazy to read go docs
                    // TODO: fix this AI cloning crap in the future
                    // 1️⃣ Collect keys
                    let mut keys: Vec<_> = hash_lit.pairs.keys().cloned().collect();
                    // 2️⃣ Sort by string representation (like Go's String())
                    keys.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
                    // 3️⃣ Compile key/value pairs
                    for key in keys {
                        let value = hash_lit.pairs.get(&key).unwrap().clone();
                        // compile key
                        self.compile(Node::Expression(key))?;

                        // compile value
                        self.compile(Node::Expression(value))?;
                    }
                    // 4️⃣ Emit OpHash
                    self.emit(Opcode::Hash, &[(hash_lit.pairs.len() * 2) as i64]);
                }
                Exp::While(while_expression) => todo!(),
            },
        }
        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        // should I clone here? I still am not sure
        Bytecode::new(
            self.scopes[self.scope_index].instructions.clone(), // current_instructions
            self.constants.clone(),
        )
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
        let scope = &mut self.scopes[self.scope_index];
        scope.previous_instruction = std::mem::replace(
            &mut scope.last_instruction,
            EmittedInstruction::new(op, pos),
        );
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos_new_instruction = self.current_instructions().len();
        self.current_instructions().0.extend(ins);
        pos_new_instruction
    }

    fn last_instruction_is(&self, op: Opcode) -> bool {
        if self.scopes[self.scope_index].instructions.is_empty() {
            return false;
        }
        self.scopes[self.scope_index].last_instruction.opcode == op
    }

    fn remove_last_pop(&mut self) {
        let last_pos = self.scopes[self.scope_index].last_instruction.position;
        let previous = self.scopes[self.scope_index].previous_instruction.clone();

        self.scopes[self.scope_index]
            .instructions
            .0
            .truncate(last_pos);
        self.scopes[self.scope_index].last_instruction = previous;
    }

    fn change_operand(&mut self, op_pos: usize, operand: i64) {
        let op: Opcode = TryFrom::try_from(self.current_instructions()[op_pos]).unwrap();
        let new_instruction = make(op, &[operand]);

        self.replace_instruction(op_pos, &new_instruction);
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: &[u8]) {
        let ins = self.current_instructions();
        let end = pos + new_instruction.len();
        ins.0[pos..end].copy_from_slice(new_instruction);
    }
    // TODO: I don't like this api for remembering previous compilations. is there any other way?
    pub fn new_with_state(s: SymbolTable, constants: Vec<Object>) -> Self {
        let mut compiler = Compiler::new();
        compiler.symbol_table = s;
        compiler.constants = constants;
        compiler
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::default();
        self.scopes.push(scope);
        self.scope_index += 1;
    }
    fn leave_scope(&mut self) -> Instructions {
        let scope = self
            .scopes
            .pop()
            .expect("There should always be a scope entered");
        self.scope_index -= 1;
        scope.instructions
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pos = self.scopes[self.scope_index].last_instruction.position;
        self.replace_instruction(last_pos, &make(Opcode::ReturnValue, &[]));

        self.scopes[self.scope_index].last_instruction.opcode = Opcode::ReturnValue;
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
    pub fn test_string_object(obj: &Object, expected: &str) -> bool {
        if let Object::String(result) = obj {
            if result.value != expected {
                eprintln!(
                    "object has wrong value. got={}, want{}",
                    result.value, expected
                );
                false
            } else {
                true
            }
        } else {
            eprintln!("object is not String. got=({:?})", obj);
            false
        }
    }

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

            test_instructions(expected_instructions, &bytecode.instructions)
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
    fn test_instructions(expected: Vec<Instructions>, actual: &Instructions) -> Result<(), String> {
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
            } else if let Some(expected_constant) = constant.downcast_ref::<&str>() {
                if !test_string_object(&actual[i], *expected_constant) {
                    return Err(format!("constant {} - test_string_object failed", i));
                }
            } else if let Some(expected_constant) = constant.downcast_ref::<Vec<Instructions>>() {
                if let Object::CompiledFunction(actual_fn) = &actual[i] {
                    if let Err(err) =
                        test_instructions(expected_constant.to_vec(), &actual_fn.instructions)
                    {
                        return Err(format!("constant {i} -test_instructions failed: {err}"));
                    }
                } else {
                    return Err(format!("constant {} not a function=({:?})", i, actual[i]));
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

    // TODO: fix this later
    #[should_panic]
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
    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            CompilerTestCase::new(
                "
let one = 1;
let two = 2;
",
                vec![Box::new(1i64), Box::new(2i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::SetGlobal, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::SetGlobal, &[1])),
                ],
            ),
            CompilerTestCase::new(
                "
let one = 1;
one;
",
                vec![Box::new(1i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::SetGlobal, &[0])),
                    Instructions::new(make(Op::GetGlobal, &[0])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "
let one = 1;
let two = one;
two;
",
                vec![Box::new(1i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::SetGlobal, &[0])),
                    Instructions::new(make(Op::GetGlobal, &[0])),
                    Instructions::new(make(Op::SetGlobal, &[1])),
                    Instructions::new(make(Op::GetGlobal, &[1])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            CompilerTestCase::new(
                r#""monkey""#,
                vec![Box::new("monkey")],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                r#""mon" + "key""#,
                vec![Box::new("mon"), Box::new("key")],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Add, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
    #[test]
    fn test_array_literals() {
        let tests = vec![
            CompilerTestCase::new(
                "[]",
                vec![],
                vec![
                    Instructions::new(make(Op::Array, &[0])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "[1, 2, 3]",
                vec![Box::new(1i64), Box::new(2i64), Box::new(3i64)],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Array, &[3])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    Box::new(1i64),
                    Box::new(2i64),
                    Box::new(3i64),
                    Box::new(4i64),
                    Box::new(5i64),
                    Box::new(6i64),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Add, &[])),
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Constant, &[3])),
                    Instructions::new(make(Op::Sub, &[])),
                    Instructions::new(make(Op::Constant, &[4])),
                    Instructions::new(make(Op::Constant, &[5])),
                    Instructions::new(make(Op::Mul, &[])),
                    Instructions::new(make(Op::Array, &[3])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
    #[test]
    fn test_hash_literals() {
        let tests = vec![
            CompilerTestCase::new(
                "{}",
                vec![],
                vec![
                    Instructions::new(make(Op::Hash, &[0])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    Box::new(1i64),
                    Box::new(2i64),
                    Box::new(3i64),
                    Box::new(4i64),
                    Box::new(5i64),
                    Box::new(6i64),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Constant, &[3])),
                    Instructions::new(make(Op::Constant, &[4])),
                    Instructions::new(make(Op::Constant, &[5])),
                    Instructions::new(make(Op::Hash, &[6])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    Box::new(1i64),
                    Box::new(2i64),
                    Box::new(3i64),
                    Box::new(4i64),
                    Box::new(5i64),
                    Box::new(6i64),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Add, &[])),
                    Instructions::new(make(Op::Constant, &[3])),
                    Instructions::new(make(Op::Constant, &[4])),
                    Instructions::new(make(Op::Constant, &[5])),
                    Instructions::new(make(Op::Mul, &[])),
                    Instructions::new(make(Op::Hash, &[4])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
    #[test]
    fn test_index_expressions() {
        let tests = vec![
            CompilerTestCase::new(
                "[1, 2, 3][1 + 1]",
                vec![
                    Box::new(1i64),
                    Box::new(2i64),
                    Box::new(3i64),
                    Box::new(1i64),
                    Box::new(1i64),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Array, &[3])),
                    Instructions::new(make(Op::Constant, &[3])),
                    Instructions::new(make(Op::Constant, &[4])),
                    Instructions::new(make(Op::Add, &[])),
                    Instructions::new(make(Op::Index, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "{1: 2}[2 - 1]",
                vec![
                    Box::new(1i64),
                    Box::new(2i64),
                    Box::new(2i64),
                    Box::new(1i64),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Constant, &[1])),
                    Instructions::new(make(Op::Hash, &[2])),
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Constant, &[3])),
                    Instructions::new(make(Op::Sub, &[])),
                    Instructions::new(make(Op::Index, &[])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
    #[test]
    fn test_functions() {
        let tests = vec![
            CompilerTestCase::new(
                "fn() {return 5 + 10 }",
                vec![
                    Box::new(5i64),
                    Box::new(10i64),
                    Box::new(vec![
                        Instructions::new(make(Op::Constant, &[0])),
                        Instructions::new(make(Op::Constant, &[1])),
                        Instructions::new(make(Op::Add, &[])),
                        Instructions::new(make(Op::ReturnValue, &[])),
                    ]),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "fn() {5 + 10 }",
                vec![
                    Box::new(5i64),
                    Box::new(10i64),
                    Box::new(vec![
                        Instructions::new(make(Op::Constant, &[0])),
                        Instructions::new(make(Op::Constant, &[1])),
                        Instructions::new(make(Op::Add, &[])),
                        Instructions::new(make(Op::ReturnValue, &[])),
                    ]),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "fn() { 1; 2}",
                vec![
                    Box::new(1i64),
                    Box::new(2i64),
                    Box::new(vec![
                        Instructions::new(make(Op::Constant, &[0])),
                        Instructions::new(make(Op::Pop, &[])),
                        Instructions::new(make(Op::Constant, &[1])),
                        Instructions::new(make(Op::ReturnValue, &[])),
                    ]),
                ],
                vec![
                    Instructions::new(make(Op::Constant, &[2])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
            CompilerTestCase::new(
                "fn() {}",
                vec![Box::new(vec![Instructions::new(make(Op::Return, &[]))])],
                vec![
                    Instructions::new(make(Op::Constant, &[0])),
                    Instructions::new(make(Op::Pop, &[])),
                ],
            ),
        ];
        run_compiler_tests(tests);
    }
    #[test]
    fn test_compiler_scopes() {
        let mut compiler = Compiler::new();
        assert_eq!(
            compiler.scope_index, 0,
            "scope_index wrong. got={}, want=0",
            compiler.scope_index
        );
        compiler.emit(Opcode::Mul, &[]);
        compiler.enter_scope();
        assert_eq!(
            compiler.scope_index, 1,
            "scope_index wrong. got={}, want=1",
            compiler.scope_index
        );
        compiler.emit(Opcode::Sub, &[]);
        let len = compiler.scopes[compiler.scope_index].instructions.len();
        assert_eq!(len, 1, "instructions length wrong. got={len}",);

        let last = compiler.scopes[compiler.scope_index]
            .last_instruction
            .clone();
        assert_eq!(
            last.opcode,
            Opcode::Sub,
            "last_instruction.opcode wrong. got={:?}, want={:?}",
            last.opcode,
            Opcode::Sub
        );
        compiler.leave_scope();
        assert_eq!(
            compiler.scope_index, 0,
            "scope_index wrong. got={}, want=0",
            compiler.scope_index
        );
        compiler.emit(Opcode::Add, &[]);
        let len = compiler.scopes[compiler.scope_index].instructions.len();
        assert_eq!(len, 2, "instructions length wrong. got={len}",);

        let last = compiler.scopes[compiler.scope_index]
            .last_instruction
            .clone();
        assert_eq!(
            last.opcode,
            Opcode::Add,
            "last_instruction.opcode wrong. got={:?}, want={:?}",
            last.opcode,
            Opcode::Add
        );
        let prev = compiler.scopes[compiler.scope_index]
            .previous_instruction
            .clone();
        assert_eq!(
            prev.opcode,
            Opcode::Mul,
            "last_instruction.opcode wrong. got={:?}, want={:?}",
            last.opcode,
            Opcode::Mul
        );
    }
}
