use crate::ast;
use crate::code::{read_u16, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::lexer::Lexer;
use crate::object::{Null, Object};
use crate::parser::Parser;

const STACKSIZE: usize = 2048;

#[derive(Debug)]
struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize, // always points to next value. top of stack is stack[sp -1]
}

impl VM {
    pub fn new(
        Bytecode {
            constants,
            instructions,
        }: Bytecode,
    ) -> Self {
        Self {
            constants,
            instructions,
            stack: vec![Object::Null(Null); STACKSIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Option<Object> {
        if self.sp == 0 {
            None
        } else {
            // TODO: does this have to be clone? probably not
            Some(self.stack[self.sp - 1].clone())
        }
    }
    pub fn pop(&mut self) -> Object {
        let o = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        o
    }
    pub fn run(&mut self) -> Result<(), String> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op: Opcode = TryFrom::try_from(self.instructions[ip])?;
            match op {
                Opcode::Constant => {
                    let const_ind = read_u16(&self.instructions[ip + 1..ip + 3]);
                    ip += 2;
                    self.push(self.constants[const_ind as usize].clone())?;
                }
                Opcode::Add => {
                    let right = self.pop();
                    let left = self.pop();
                    let result = match (left, right) {
                        (Object::Integer(left_val), Object::Integer(right_val)) => {
                            left_val.value + right_val.value
                        }

                        _ => todo!("this add is not yet supported"),
                    };
                    self.push(Object::new_int_var(result))?
                }
                _ => panic!("unknown instruction"),
            }
            ip += 1;
        }
        Ok(())
    }

    pub fn push(&mut self, o: Object) -> Result<(), String> {
        if self.sp >= STACKSIZE {
            Err("stack overflow".to_string())
        } else {
            self.stack[self.sp] = o;
            self.sp += 1;
            Ok(())
        }
    }
}

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
        fn new(input: impl Into<String>, expected: Box<dyn Any>) -> Self {
            Self {
                input: input.into(),
                expected,
            }
        }
    }
    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase::new("1", Box::new(1i64)),
            VmTestCase::new("2", Box::new(2i64)),
            VmTestCase::new("1 + 2", Box::new(3i64)),
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
            let stack_elm = vm.stack_top().expect("stack shouldn't be empty");
            test_expected_object(expected.as_ref(), &stack_elm);
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
