use crate::ast;
use crate::code::{read_u16, read_u8, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::lexer::Lexer;
use crate::object::{
    native_bool_to_boolean_object, Array, CompiledFunctionObj, HashObj, HashPair, Hashable,
    Integer, IsHashable, Null, Object, ObjectTrait, FALSE, GARBAGEVALOBJ, NULL, TRUE,
};
use crate::parser::Parser;
use std::array;
use std::collections::HashMap;

const STACKSIZE: usize = 2048;
pub const GLOBALSSIZE: usize = 65536;
const MAXFRAMES: usize = 1024;

#[derive(Debug, Clone)]
struct Frame {
    comp_fn: CompiledFunctionObj,
    ip: i64,
    bp: i64,
}
impl Frame {
    fn new(comp_fn: CompiledFunctionObj, bp: i64) -> Self {
        Self {
            comp_fn,
            ip: -1,
            bp: bp,
        }
    }
    fn garbage_value() -> Self {
        Self {
            comp_fn: Default::default(),
            ip: -1,
            bp: -1,
        }
    }
    fn instructions(&mut self) -> &mut Instructions {
        &mut self.comp_fn.instructions
    }
}

#[derive(Debug)]
pub struct VM {
    constants: Vec<Object>,
    stack: Vec<Object>,
    sp: usize, // always points to next value. top of stack is stack[sp -1]
    //pc: usize // TODO: implement in future
    // TODO: so I should probably use Rc<Refcell<Object>> more often becaue I am just cloning
    // everything a lot but I am too lazy. I had to use it here since I need a pointer with small
    // size to be stored on the stack and object is on the heap from what I understand
    globals: Vec<Object>,
    frames: Vec<Frame>,
    frames_index: usize,
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
        let main_fn = CompiledFunctionObj::new(instructions, 0); // I am not sure what to have here
        let main_frame = Frame::new(main_fn, 0);

        let mut frames = vec![Frame::garbage_value(); MAXFRAMES];
        frames[0] = main_frame;
        Self {
            constants,
            stack: vec![GARBAGEVALOBJ; STACKSIZE],
            sp: 0,
            globals: vec![GARBAGEVALOBJ; GLOBALSSIZE],
            frames: frames,
            frames_index: 1,
        }
    }
    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }
    fn push_frame(&mut self, f: Frame) {
        self.frames[self.frames_index] = f;
        self.frames_index += 1;
    }
    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        self.frames[self.frames_index].clone()
    }

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
        match (left, right) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                self.execute_binary_integer_operation(op, left_val.value, right_val.value)
            }
            // TODO: maybe I should have let the other side free to allow string + int
            (Object::String(left_val), Object::String(right_val)) => {
                self.execute_binary_string_operation(op, left_val.value, right_val.value)
            }
            (left, right) => todo!(
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
    fn execute_binary_string_operation(
        &mut self,
        op: Opcode,
        left: String,
        right: String,
    ) -> Result<(), String> {
        let result = match op {
            Opcode::Add => format!("{}{}", left, right),
            _ => return Err(format!("unknown integer operator: {:?}", op)),
        };
        self.push(Object::new_str_var(&result))
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
    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<(), String> {
        match (left, index) {
            (Object::Arr(arr_obj), Object::Integer(int_obj)) => {
                self.execute_array_index(arr_obj, int_obj)
            }
            (Object::Hash(hash_obj), index) => self.execute_hash_index(hash_obj, index),
            (left, right) => todo!(
                "unsupported types for index operation: {} {}",
                left.r#type(),
                right.r#type()
            ),
        }
    }
    fn execute_array_index(&mut self, arr_obj: Array, int_obj: Integer) -> Result<(), String> {
        let i = int_obj.value;
        let max = arr_obj.elements.len() as i64 - 1;
        if i < 0 || i > max {
            self.push(NULL)
        } else {
            self.push(arr_obj.elements[i as usize].clone())
        }
    }
    fn execute_hash_index(&mut self, hash_obj: HashObj, index: Object) -> Result<(), String> {
        index.is_hashable()?;
        let pair = hash_obj
            .pairs
            .get(&index.hash_key())
            .ok_or("key doesn't exist");
        match pair {
            Ok(pair) => self.push(pair.val.clone()),
            Err(err) => self.push(NULL),
        }
    }
    pub fn run(&mut self) -> Result<(), String> {
        // TODO: rename ip to pc and make it a struct field in order to make the step function for
        // the VM
        let mut ip: i64; // the instruction pointer is not fancy in this vm
        let mut ins;
        let mut op: Opcode;
        while self.current_frame().ip < (self.current_frame().instructions().len() as i64 - 1) {
            self.current_frame().ip += 1;

            ip = self.current_frame().ip;
            ins = self.current_frame().instructions();
            op = TryFrom::try_from(ins[ip as usize])?;
            match op {
                Opcode::Constant => {
                    let const_ind = read_u16(&ins[ip as usize + 1..ip as usize + 3]);
                    self.current_frame().ip += 2;
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
                    let pos = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                    self.current_frame().ip = pos as i64 - 1; // minus 1 is because counter is always incremented
                }
                Opcode::JumpNotTruthy => {
                    let pos = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                    self.current_frame().ip += 2;
                    let condition = self.pop();
                    if !condition.is_truthy() {
                        self.current_frame().ip = pos as i64 - 1;
                    }
                }
                Opcode::Null => self.push(NULL)?,
                Opcode::SetGlobal => {
                    let glob_ind = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                    self.current_frame().ip += 2;
                    self.globals[glob_ind] = self.pop();
                }
                Opcode::GetGlobal => {
                    let glob_ind = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                    self.current_frame().ip += 2;

                    self.push(self.globals[glob_ind].clone())?;
                }
                Opcode::Array => {
                    let num_elements = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                    self.current_frame().ip += 2;

                    let array = self.build_array(self.sp - num_elements, self.sp);
                    self.sp = self.sp - num_elements;
                    self.push(array)?;
                }
                Opcode::Hash => {
                    let num_elements = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                    self.current_frame().ip += 2;

                    let hash = self.build_hash(self.sp - num_elements, self.sp)?;
                    self.sp = self.sp - num_elements;
                    self.push(hash)?;
                }
                Opcode::Index => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(left, index)?;
                }
                Opcode::Call => {
                    if let Object::CompiledFunction(ref comp_fn) = self.stack[self.sp - 1] {
                        let frame = Frame::new(comp_fn.clone(), self.sp.try_into().unwrap());
                        let next_sp: usize = frame.bp as usize + comp_fn.num_locals;
                        self.push_frame(frame);
                        self.sp = next_sp;
                    } else {
                        // note that in error case top of stack is not popped
                        return Err("calling non-function".to_string());
                    }
                }
                Opcode::ReturnValue => {
                    let return_value = self.pop();

                    let frame = self.pop_frame();
                    self.sp = usize::try_from(frame.bp).unwrap() - 1; // - 1 instead of popping the function obj

                    self.push(return_value)?;
                }
                Opcode::Return => {
                    let frame = self.pop_frame();
                    self.sp = usize::try_from(frame.bp).unwrap() - 1; // - 1 instead of popping the function obj

                    self.push(NULL)?;
                }
                Opcode::SetLocal => {
                    let local_ind = ins[usize::try_from(ip + 1).unwrap()] as usize;
                    self.current_frame().ip += 1;

                    let frame_bp: usize = self.current_frame().bp.try_into().unwrap();
                    let local_var_val = self.pop();
                    self.stack[frame_bp + local_ind] = local_var_val;
                }
                Opcode::GetLocal => {
                    dbg!(&ins);
                    let local_ind = ins[usize::try_from(dbg!(ip + 1)).unwrap()] as usize;
                    self.current_frame().ip += 1;

                    let frame_bp: usize = dbg!(self.current_frame().bp.try_into()).unwrap();
                    dbg!(local_ind);
                    self.push(self.stack[frame_bp + local_ind].clone())?;
                }

                _ => panic!("unknown instruction"),
            }
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

    fn build_array(&mut self, start_ind: usize, end_ind: usize) -> Object {
        let mut elements = vec![NULL; end_ind - start_ind];
        let mut i = start_ind;
        while i < end_ind {
            elements[i - start_ind] = self.stack[i].clone();
            i += 1;
        }
        Object::new_array_var(elements)
    }
    fn build_hash(&mut self, start_ind: usize, end_ind: usize) -> Result<Object, String> {
        let mut hashed_pairs = HashMap::new();
        let mut i = start_ind;
        while i < end_ind {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            // TODO: this hash_key should return result instead
            key.is_hashable()?;
            let hash_key = key.hash_key();

            let pair = HashPair::new(key, value);
            hashed_pairs.insert(hash_key, pair);

            i += 2;
        }
        Ok(Object::new_hash_var(hashed_pairs))
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
    use std::{any::Any, collections::HashMap};

    use crate::{
        compiler::{self, Compiler},
        object::{HashKey, Hashable},
    };

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
        } else if let Some(expec) = expected.downcast_ref::<&str>() {
            test_string_object(*expec, actual)
                .unwrap_or_else(|e| panic!("test_string_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<Null>() {
            test_null_object(actual).unwrap_or_else(|e| panic!("test_null_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<Vec<i64>>() {
            test_arr_ints_object(&expec, actual)
                .unwrap_or_else(|e| panic!("test_arr_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<HashMap<HashKey, i64>>() {
            test_hash_ints_object(&expec, actual)
                .unwrap_or_else(|e| panic!("test_arr_object failed: {e}"));
        } else {
            panic!("unknown dyn object: {:?}", expected);
        }
    }
    pub fn test_hash_ints_object(
        expected: &HashMap<HashKey, i64>,
        actual: &Object,
    ) -> Result<(), String> {
        if let Object::Hash(result) = actual {
            if result.pairs.len() != expected.len() {
                Err(format!(
                    "hash has wrong number of pairs. want={}, got={}",
                    expected.len(),
                    result.pairs.len()
                ))
            } else {
                for (expected_key, expected_value) in expected {
                    let pair = result
                        .pairs
                        .get(expected_key)
                        .ok_or("no pair for given key in pairs")?;
                    test_integer_object(*expected_value, &pair.val)?;
                }
                Ok(())
            }
        } else {
            Err(format!("object not Hash. got=({:?})", actual))
        }
    }
    pub fn test_arr_ints_object(expected: &[i64], actual: &Object) -> Result<(), String> {
        if let Object::Arr(result) = actual {
            if result.elements.len() != expected.len() {
                Err(format!(
                    "wrong num of elements. want={}, got={}",
                    expected.len(),
                    result.elements.len()
                ))
            } else {
                for (i, expected_elm) in expected.iter().enumerate() {
                    test_integer_object(*expected_elm, &result.elements[i])?;
                }
                Ok(())
            }
        } else {
            Err(format!("object not Array. got=({:?})", actual))
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
    // TODO: remove one of duplicated function
    pub fn test_string_object(expected: &str, actual: &Object) -> Result<(), String> {
        if let Object::String(result) = actual {
            if result.value != expected {
                Err(format!(
                    "object has wrong value. got={}, want={}",
                    result.value, expected
                ))
            } else {
                Ok(())
            }
        } else {
            Err(format!("object is not String. got=({:?})", actual))
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
    #[test]
    fn test_string_expressions() {
        let tests = vec![
            VmTestCase::new(r#""monkey""#, Box::new("monkey")),
            VmTestCase::new(r#""mon" + "key""#, Box::new("monkey")),
            VmTestCase::new(r#""mon" + "key" + "banana""#, Box::new("monkeybanana")),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_array_literals() {
        let tests = vec![
            VmTestCase::new("[]", Box::new(vec![0 as i64; 0])),
            VmTestCase::new("[1, 2, 3]", Box::new(vec![1i64, 2, 3])),
            VmTestCase::new("[1 + 2, 3 * 4, 5 + 6]", Box::new(vec![3i64, 12, 11])),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_hash_literals() {
        let tests = vec![
            VmTestCase::new("{}", Box::new(HashMap::<HashKey, i64>::new())),
            VmTestCase::new(
                "{1: 2, 2: 3}",
                Box::new(HashMap::from([
                    (Object::new_int_var(1).hash_key(), 2i64),
                    (Object::new_int_var(2).hash_key(), 3i64),
                ])),
            ),
            VmTestCase::new(
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Box::new(HashMap::from([
                    (Object::new_int_var(2).hash_key(), 4i64),
                    (Object::new_int_var(6).hash_key(), 16i64),
                ])),
            ),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_index_expressions() {
        let tests = vec![
            VmTestCase::new("[1, 2, 3][1]", Box::new(2i64)),
            VmTestCase::new("[1, 2, 3][0 + 2]", Box::new(3i64)),
            VmTestCase::new("[[1, 1, 1]][0][0]", Box::new(1i64)),
            VmTestCase::new("[][0]", Box::new(Null)),
            VmTestCase::new("[1, 2, 3][99]", Box::new(Null)),
            VmTestCase::new("[1][-1]", Box::new(Null)),
            VmTestCase::new("{1: 1, 2: 2}[1]", Box::new(1i64)),
            VmTestCase::new("{1: 1, 2: 2}[2]", Box::new(2i64)),
            VmTestCase::new("{1: 1}[0]", Box::new(Null)),
            VmTestCase::new("{}[0]", Box::new(Null)),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_calling_functions_without_arguments() {
        let tests = vec![
            VmTestCase::new(
                "let fivePlusTen = fn() { 5 + 10; };
fivePlusTen();",
                Box::new(15i64),
            ),
            VmTestCase::new(
                "let one = fn() { 1; };
let two = fn() { 2; };
one() + two()",
                Box::new(3i64),
            ),
            VmTestCase::new(
                "let a = fn() { 1 };
let b = fn() { a() + 1 };
let c = fn() { b() + 1 };
c();",
                Box::new(3i64),
            ),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_functions_with_return_statement() {
        let tests = vec![
            VmTestCase::new(
                "let earlyExit = fn() { return 99; 100; };
earlyExit();",
                Box::new(99i64),
            ),
            VmTestCase::new(
                "let earlyExit = fn() { return 99; return 100; };
earlyExit();",
                Box::new(99i64),
            ),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_functions_without_return_value() {
        let tests = vec![
            VmTestCase::new(
                "let noReturn = fn() { };
noReturn();",
                Box::new(Null),
            ),
            VmTestCase::new(
                "let noReturn = fn() { };
let noReturnTwo = fn() { noReturn(); };
noReturn();
noReturnTwo();",
                Box::new(Null),
            ),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_first_class_functions() {
        let tests = vec![
            VmTestCase::new(
                "let returnsOne = fn() { 1; };
            let returnsOneReturner = fn() { returnsOne; };
            returnsOneReturner()();",
                Box::new(1i64),
            ),
            VmTestCase::new(
                "let returnsOneReturner = fn() {
let returnsOne = fn() { 1; };
returnsOne;
};
returnsOneReturner()();",
                Box::new(1i64),
            ),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_calling_functions_with_bindings() {
        let tests = vec![
            VmTestCase::new(
                "let one = fn() { let one = 1; one };
            one();",
                Box::new(1i64),
            ),
            VmTestCase::new(
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
            oneAndTwo();",
                Box::new(3i64),
            ),
            VmTestCase::new(
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                        let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
                        oneAndTwo() + threeAndFour();",
                Box::new(10i64),
            ),
            VmTestCase::new(
                "let firstFoobar = fn() { let foobar = 50; foobar; };
                        let secondFoobar = fn() { let foobar = 100; foobar; };
                        firstFoobar() + secondFoobar();",
                Box::new(150i64),
            ),
            VmTestCase::new(
                "let globalSeed = 50;
            let minusOne = fn() {
            let num = 1;
            globalSeed - num;
            }
            let minusTwo = fn() {
            let num = 2;
            globalSeed - num;
            }
            minusOne() + minusTwo();",
                Box::new(97i64),
            ),
        ];
        run_vm_tests(tests);
    }
}
