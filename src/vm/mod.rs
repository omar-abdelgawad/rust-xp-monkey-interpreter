use crate::ast;
use crate::code::{read_u16, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::lexer::Lexer;
use crate::object::{
    native_bool_to_boolean_object, Null, Object, ObjectTrait, FALSE, GARBAGEVALOBJ, NULL, TRUE,
};
use crate::parser::Parser;
use std::array;

const STACKSIZE: usize = 2048;
pub const GLOBALSSIZE: usize = 65536;

#[derive(Debug)]
pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize, // always points to next value. top of stack is stack[sp -1]
    //pc: usize // TODO: implement in future
    // TODO: so I should probably use Rc<Refcell<Object>> more often becaue I am just cloning
    // everything a lot but I am too lazy. I had to use it here since I need a pointer with small
    // size to be stored on the stack and object is on the heap from what I understand
    globals: Vec<Object>,
}

impl VM {
    pub fn globals(&self) -> Vec<Object> {
        self.globals.clone()
    }
    pub fn new(
        Bytecode {
            constants,
            instructions,
        }: Bytecode,
    ) -> Self {
        Self {
            constants,
            instructions,
            stack: vec![GARBAGEVALOBJ; STACKSIZE],
            sp: 0,
            globals: vec![GARBAGEVALOBJ; GLOBALSSIZE],
        }
    }

    //pub fn stack_top(&self) -> Option<Object> {
    //    if self.sp == 0 {
    //        None
    //    } else {
    //        // TODO: does this have to be clone? probably not
    //        Some(self.stack[self.sp - 1].clone())
    //    }
    //}
    pub fn last_popped_stack_elem(&self) -> Object {
        self.stack[self.sp].clone()
    }
    pub fn pop(&mut self) -> Object {
        let o = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        o
    }
    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                self.execute_binary_integer_operation(op, left_val.value, right_val.value)
            }
            _ => todo!(
                "unsupported types for binary operation: {} {}",
                left.r#type(),
                right.r#type()
            ),
        }
    }
    fn execute_binary_integer_operation(
        &mut self,
        op: Opcode,
        left: i64,
        right: i64,
    ) -> Result<(), String> {
        let result = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => return Err(format!("unknown integer operator: {:?}", op)),
        };
        self.push(Object::new_int_var(result))
    }
    fn execute_comparison(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                self.execute_integer_comparison(op, left_val.value, right_val.value)
            }
            (Object::Boolean(left_val), Object::Boolean(right_val)) => {
                self.execute_boolean_comparison(op, left_val.value, right_val.value)
            }
            _ => todo!(
                "unsupported types for binary operation: {} {}",
                left.r#type(),
                right.r#type()
            ),
        }
    }
    fn execute_integer_comparison(
        &mut self,
        op: Opcode,
        left: i64,
        right: i64,
    ) -> Result<(), String> {
        let val = match op {
            Opcode::Equal => left == right,
            Opcode::NotEqual => left != right,
            Opcode::GreaterThan => left > right,
            _ => return Err(format!("unknown operator: {op:?}")),
        };
        self.push(native_bool_to_boolean_object(val))
    }
    fn execute_boolean_comparison(
        &mut self,
        op: Opcode,
        left: bool,
        right: bool,
    ) -> Result<(), String> {
        let val = match op {
            Opcode::Equal => left == right,
            Opcode::NotEqual => left != right,
            _ => return Err(format!("unknown operator: {op:?}")),
        };
        self.push(native_bool_to_boolean_object(val))
    }
    fn execute_bang_operator(&mut self) -> Result<(), String> {
        let right = self.pop();
        match (right) {
            TRUE => self.push(FALSE),
            FALSE => self.push(TRUE),
            NULL => self.push(TRUE), // negation of NULL is true now even though it is still truthy
            _ => self.push(FALSE),   // anything other than false is "truthy"
        }
    }
    fn execute_minus_operator(&mut self) -> Result<(), String> {
        let right = self.pop();
        match (right) {
            Object::Integer(right_val) => self.push(Object::new_int_var(-right_val.value)),
            _ => todo!("unsupported type for negation: {}", right.r#type()),
        }
    }
    pub fn run(&mut self) -> Result<(), String> {
        // TODO: rename ip to pc and make it a struct field in order to make the step function for
        // the VM
        let mut ip = 0; // the instruction pointer is not fancy in this vm
        while ip < self.instructions.len() {
            let op: Opcode = TryFrom::try_from(self.instructions[ip])?;
            match op {
                Opcode::Constant => {
                    let const_ind = read_u16(&self.instructions[ip + 1..ip + 3]);
                    ip += 2;
                    self.push(self.constants[const_ind as usize].clone())?;
                }
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(op)?
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::True => self.push(TRUE)?,
                Opcode::False => self.push(FALSE)?,
                Opcode::Equal | Opcode::NotEqual | Opcode::GreaterThan => {
                    self.execute_comparison(op)?
                }
                Opcode::Bang => self.execute_bang_operator()?,
                Opcode::Minus => self.execute_minus_operator()?,
                Opcode::Jump => {
                    let pos = read_u16(&self.instructions[ip + 1..ip + 3]) as usize;
                    ip = pos - 1; // minus 1 is because counter is always incremented
                }
                Opcode::JumpNotTruthy => {
                    let pos = read_u16(&self.instructions[ip + 1..ip + 3]) as usize;
                    ip += 2;
                    let condition = self.pop();
                    if !condition.is_truthy() {
                        ip = pos - 1;
                    }
                }
                Opcode::Null => self.push(NULL)?,
                Opcode::SetGlobal => {
                    let glob_ind = read_u16(&self.instructions[ip + 1..ip + 3]) as usize;
                    ip += 2;
                    self.globals[glob_ind] = self.pop();
                }
                Opcode::GetGlobal => {
                    let glob_ind = read_u16(&self.instructions[ip + 1..ip + 3]) as usize;
                    ip += 2;

                    self.push(self.globals[glob_ind].clone())?;
                }

                _ => panic!("unknown instruction"),
            }
            ip += 1;
        }
        Ok(())
    }

    pub fn push(&mut self, o: Object) -> Result<(), String> {
        if self.sp >= STACKSIZE {
            Err("woops! stack overflow".to_string())
        } else {
            self.stack[self.sp] = o;
            self.sp += 1;
            Ok(())
        }
    }
    pub fn new_with_globals_store(bytecode: Bytecode, s: Vec<Object>) -> Self {
        let mut vm = VM::new(bytecode);
        vm.globals = s;
        vm
    }
}

// TODO: remove this copied function
fn parse(input: String) -> ast::Program {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    p.parse_program()
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
        fn new(input: impl Into<String>, expected: Box<dyn Any>) -> Self {
            Self {
                input: input.into(),
                expected,
            }
        }
    }
    #[test]
    fn test_boolean_expression() {
        let tests = vec![
            VmTestCase::new("true", Box::new(true)),
            VmTestCase::new("false", Box::new(false)),
            VmTestCase::new("1 < 2", Box::new(true)),
            VmTestCase::new("1 > 2", Box::new(false)),
            VmTestCase::new("1 < 1", Box::new(false)),
            VmTestCase::new("1 > 1", Box::new(false)),
            VmTestCase::new("1 == 1", Box::new(true)),
            VmTestCase::new("1 != 1", Box::new(false)),
            VmTestCase::new("1 == 2", Box::new(false)),
            VmTestCase::new("1 != 2", Box::new(true)),
            VmTestCase::new("true == true", Box::new(true)),
            VmTestCase::new("false == false", Box::new(true)),
            VmTestCase::new("true == false", Box::new(false)),
            VmTestCase::new("true != false", Box::new(true)),
            VmTestCase::new("false != true", Box::new(true)),
            VmTestCase::new("(1 < 2) == true", Box::new(true)),
            VmTestCase::new("(1 < 2) == false", Box::new(false)),
            VmTestCase::new("(1 > 2) == true", Box::new(false)),
            VmTestCase::new("(1 > 2) == false", Box::new(true)),
            VmTestCase::new("!true", Box::new(false)),
            VmTestCase::new("!false", Box::new(true)),
            VmTestCase::new("!5", Box::new(false)),
            VmTestCase::new("!!true", Box::new(true)),
            VmTestCase::new("!!false", Box::new(false)),
            VmTestCase::new("!!5", Box::new(true)),
            VmTestCase::new("!(if (false) { 5; })", Box::new(true)),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase::new("1", Box::new(1i64)),
            VmTestCase::new("2", Box::new(2i64)),
            VmTestCase::new("1 + 2", Box::new(3i64)),
            VmTestCase::new("1 - 2", Box::new(-1i64)),
            VmTestCase::new("1 * 2", Box::new(2i64)),
            VmTestCase::new("4 / 2", Box::new(2i64)),
            VmTestCase::new("50 / 2 * 2 + 10 - 5", Box::new(55i64)),
            VmTestCase::new("5 + 5 + 5 + 5 - 10", Box::new(10i64)),
            VmTestCase::new("2 * 2 * 2 * 2 * 2", Box::new(32i64)),
            VmTestCase::new("5 * 2 + 10", Box::new(20i64)),
            VmTestCase::new("5 + 2 * 10", Box::new(25i64)),
            VmTestCase::new("5 * (2 + 10)", Box::new(60i64)),
            VmTestCase::new("-5", Box::new(-5i64)),
            VmTestCase::new("-10", Box::new(-10i64)),
            VmTestCase::new("-50 + 100 + -50", Box::new(0i64)),
            VmTestCase::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", Box::new(50i64)),
        ];
        run_vm_tests(tests);
    }
    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for VmTestCase { input, expected } in tests {
            let program = parse(input);
            let mut comp = Compiler::new();
            comp.compile(ast::Node::Program(program))
                .unwrap_or_else(|e| panic!("compiler error: {e:?}"));
            let mut vm = VM::new(comp.bytecode());
            vm.run().unwrap_or_else(|e| panic!("vm error: {e}"));
            let stack_elm = vm.last_popped_stack_elem();
            test_expected_object(expected.as_ref(), &stack_elm);
        }
    }

    fn test_expected_object(expected: &dyn Any, actual: &Object) {
        if let Some(expec) = expected.downcast_ref::<i64>() {
            test_integer_object(*expec, actual)
                .unwrap_or_else(|e| panic!("test_integer_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<bool>() {
            test_boolean_object(*expec, actual)
                .unwrap_or_else(|e| panic!("test_boolean_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<Null>() {
            test_null_object(actual).unwrap_or_else(|e| panic!("test_null_object failed: {e}"));
        } else {
            panic!("unknown dyn object: {:?}", expected);
        }
    }
    // TODO: remove one of duplicated function
    pub fn test_integer_object(expected: i64, actual: &Object) -> Result<(), String> {
        if let Object::Integer(result) = actual {
            if result.value != expected {
                Err(format!(
                    "object has wrong value. got={}, want={}",
                    result.value, expected
                ))
            } else {
                Ok(())
            }
        } else {
            Err(format!("object is not Integer. got=({:?})", actual))
        }
    }
    pub fn test_boolean_object(expected: bool, actual: &Object) -> Result<(), String> {
        if let Object::Boolean(result) = actual {
            if result.value != expected {
                Err(format!(
                    "object has wrong value. got={}, want={}",
                    result.value, expected
                ))
            } else {
                Ok(())
            }
        } else {
            Err(format!("object is not Boolean. got=({:?})", actual))
        }
    }
    pub fn test_null_object(actual: &Object) -> Result<(), String> {
        if let Object::Null(result) = actual {
            Ok(())
        } else {
            Err(format!("object is not Null. got=({:?})", actual))
        }
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VmTestCase::new("1 > 1", Box::new(false)),
            VmTestCase::new("if (true) { 10 }", Box::new(10i64)),
            VmTestCase::new("if (true) { 10 } else { 20 }", Box::new(10i64)),
            VmTestCase::new("if (false) { 10 } else { 20 } ", Box::new(20i64)),
            VmTestCase::new("if (1) { 10 }", Box::new(10i64)),
            VmTestCase::new("if (1 < 2) { 10 }", Box::new(10i64)),
            VmTestCase::new("if (1 < 2) { 10 } else { 20 }", Box::new(10i64)),
            VmTestCase::new("if (1 > 2) { 10 } else { 20 }", Box::new(20i64)),
            VmTestCase::new("if (1 > 2) { 10 }", Box::new(Null)),
            VmTestCase::new("if (false) { 10 }", Box::new(Null)),
            VmTestCase::new(
                "if ((if (false) { 10 })) { 10 } else { 20 }",
                Box::new(20i64),
            ),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            VmTestCase::new("let one = 1; one", Box::new(1i64)),
            VmTestCase::new("let one = 1; let two = 2; one + two", Box::new(3i64)),
            VmTestCase::new(
                "let one = 1; let two = one + one; one + two",
                Box::new(3i64),
            ),
        ];
        //dbg!(std::mem::size_of::<Object>()); // 88 bytes
        //dbg!(std::mem::size_of::<Rc<RefCell<Object>>>()); /8 bytes
        run_vm_tests(tests);
    }
}
