use crate::code::{read_u16, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::object::builtins::BUILTINS;
use crate::object::{
    false_obj, garbage_obj, native_bool_to_boolean_object, null_obj, true_obj, Array, BuiltinObj,
    ClosureObj, CompiledFunctionObj, HashObj, HashPair, Hashable, Integer, IsHashable, ObjRef,
    Object, ObjectTrait,
};
use std::{collections::HashMap, rc::Rc};

const STACKSIZE: usize = 1 << 11;
pub const GLOBALSSIZE: usize = 1 << 16;
const MAXFRAMES: usize = 1 << 10;

#[derive(Debug, Clone)]
struct Frame {
    cl: ClosureObj,
    ip: i64,
    bp: i64,
}
impl Frame {
    fn new(cl: ClosureObj, bp: i64) -> Self {
        Self { cl, ip: -1, bp }
    }
    fn garbage_value() -> Self {
        Self {
            cl: Default::default(),
            ip: -1,
            bp: -1,
        }
    }
    //fn instructions_mut(&mut self) -> &mut Instructions {
    //    &mut self.cl.comp_fn.instructions
    //}

    fn instructions(&self) -> &Instructions {
        &self.cl.comp_fn.instructions
    }
}

#[derive(Debug)]
pub struct VM {
    constants: Vec<ObjRef>,
    // TODO: make stack and its sp one struct to encapsulate their function
    stack: Vec<ObjRef>,
    sp: usize, // always points to next value. top of stack is stack[sp -1]
    // TODO: so I should probably use Rc<Refcell<Object>> more often becaue I am just cloning
    // everything a lot but I am too lazy. I had to use it here since I need a pointer with small
    // size to be stored on the stack and object is on the heap from what I understand
    globals: Vec<ObjRef>,
    frames: Vec<Frame>,
}

impl VM {
    // this function could be removed once we make an interpreter struct and remove the ugly way we
    // are currently doing interactive repl
    pub fn globals(&self) -> Vec<ObjRef> {
        self.globals.clone()
    }

    pub fn new(
        Bytecode {
            constants,
            instructions,
        }: Bytecode,
    ) -> Self {
        let main_fn = CompiledFunctionObj::new(instructions, 0, 0); // I am not sure how to init here
        let main_closure = ClosureObj::new(main_fn, vec![]);
        let main_frame = Frame::new(main_closure, 0);

        let mut frames = vec![Frame::garbage_value(); 1];
        frames[0] = main_frame;
        Self {
            constants,
            stack: vec![garbage_obj(); STACKSIZE],
            sp: 0,
            globals: vec![garbage_obj(); GLOBALSSIZE],
            frames,
        }
    }

    pub fn last_popped_stack_elem(&self) -> ObjRef {
        self.stack[self.sp].clone()
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().expect("at least main exists")
    }
    fn current_frame(&self) -> &Frame {
        self.frames.last().expect("at least main exists")
    }

    fn push_frame(&mut self, f: Frame) {
        if self.frames.len() >= MAXFRAMES {
            panic!("frame stack overflow, trying to push_frame while having max number")
        }
        self.frames.push(f);
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames.pop().expect("at least main exists")
    }

    fn push_closure(&mut self, const_ind: usize, num_free: usize) -> Result<(), String> {
        let constant = self.constants[const_ind].clone();
        if let Object::CompiledFunction(function) = &*constant {
            let mut free = Vec::with_capacity(num_free);
            // TODO: a lot of unnecessary cloning here
            for i in 0..num_free {
                free.push(self.stack[self.sp - num_free + i].clone());
            }
            self.sp -= num_free;
            let closure = ClosureObj::new(function.clone(), free);
            self.push(Rc::new(Object::Closure(closure)))
        } else {
            Err(format!("not a function: {constant:?}"))
        }
    }

    fn pop(&mut self) -> ObjRef {
        let o = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        o
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();
        match (&*left, &*right) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                self.execute_binary_integer_operation(op, left_val.value, right_val.value)
            }
            (Object::String(left_val), Object::String(right_val)) => {
                self.execute_binary_string_operation(op, &left_val.value, &right_val.value)
            }
            (Object::String(left_val), Object::Integer(right_val)) if op == Opcode::Add => {
                let result = format!("{}{}", left_val.value, right_val.value);
                self.push(Object::new_str_var(&result))
            }
            (left, right) => Err(format!(
                "unsupported types for binary operation: {} {}",
                left.r#type(),
                right.r#type()
            )),
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
        left: &str,
        right: &str,
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
        match (&*left, &*right) {
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
        match &*right {
            Object::Boolean(b) if b.value => self.push(false_obj()),
            Object::Boolean(b) if !b.value => self.push(true_obj()),
            Object::Null(_) => self.push(true_obj()), // negation of NULL is true now even though it is still truthy
            _ => self.push(false_obj()),              // anything other than false is "truthy"
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), String> {
        let right = self.pop();
        match &*right {
            Object::Integer(right_val) => self.push(Object::new_int_var(-right_val.value)),
            _ => todo!("unsupported type for negation: {}", right.r#type()),
        }
    }

    fn execute_index_expression(&mut self, left: ObjRef, index: ObjRef) -> Result<(), String> {
        match (&*left, &*index) {
            (Object::Arr(arr_obj), Object::Integer(int_obj)) => {
                self.execute_array_index(arr_obj, int_obj)
            }
            (Object::Hash(hash_obj), _index) => self.execute_hash_index(hash_obj, index.clone()),
            (left, right) => todo!(
                "unsupported types for index operation: {} {}",
                left.r#type(),
                right.r#type()
            ),
        }
    }

    fn execute_array_index(&mut self, arr_obj: &Array, int_obj: &Integer) -> Result<(), String> {
        let i = int_obj.value;
        let max = arr_obj.elements.len() as i64 - 1;
        if i < 0 || i > max {
            self.push(null_obj())
        } else {
            self.push(arr_obj.elements[i as usize].clone())
        }
    }

    fn execute_hash_index(&mut self, hash_obj: &HashObj, index: ObjRef) -> Result<(), String> {
        index.is_hashable()?;
        let pair = hash_obj
            .pairs
            .get(&index.hash_key())
            .ok_or("key doesn't exist");
        // TODO: this seems like a good place to have an error type
        match pair {
            Ok(pair) => self.push(pair.val.clone()),
            Err(err_str) => self.push(Object::new_error_var(err_str)),
        }
    }

    pub fn step(&mut self) -> Result<(), String> {
        self.current_frame_mut().ip += 1;
        let ip: i64 = self.current_frame().ip;
        let ins = self.current_frame().instructions();
        let op: Opcode = TryFrom::try_from(ins[ip as usize])?;
        match op {
            Opcode::Constant => {
                let const_ind = read_u16(&ins[ip as usize + 1..ip as usize + 3]);
                self.current_frame_mut().ip += 2;
                self.push(self.constants[const_ind as usize].clone())
            }
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                self.execute_binary_operation(op)
            }
            Opcode::Pop => {
                self.pop();
                Ok(())
            }
            Opcode::True => self.push(true_obj()),
            Opcode::False => self.push(false_obj()),
            Opcode::Equal | Opcode::NotEqual | Opcode::GreaterThan => self.execute_comparison(op),
            Opcode::Bang => self.execute_bang_operator(),
            Opcode::Minus => self.execute_minus_operator(),
            Opcode::Jump => {
                let pos = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                self.current_frame_mut().ip = pos as i64 - 1; // minus 1 is because counter is always incremented
                Ok(())
            }
            Opcode::JumpNotTruthy => {
                let pos = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                self.current_frame_mut().ip += 2;
                let condition = self.pop();
                if !condition.is_truthy() {
                    self.current_frame_mut().ip = pos as i64 - 1;
                }
                Ok(())
            }
            Opcode::Null => self.push(null_obj()),
            Opcode::SetGlobal => {
                // FIX: refer to test_redeclaring_globals_still_doesnt_shadow_old_ones for
                // documentation
                let glob_ind = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                self.current_frame_mut().ip += 2;
                self.globals[glob_ind] = self.pop();
                Ok(())
            }
            Opcode::GetGlobal => {
                let glob_ind = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                self.current_frame_mut().ip += 2;

                self.push(self.globals[glob_ind].clone())
            }
            Opcode::Array => {
                let num_elements = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                self.current_frame_mut().ip += 2;

                let array = self.build_array(self.sp - num_elements, self.sp);
                self.sp -= num_elements;
                self.push(array)
            }
            Opcode::Hash => {
                let num_elements = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                self.current_frame_mut().ip += 2;

                let hash = self.build_hash(self.sp - num_elements, self.sp)?;
                self.sp -= num_elements;
                self.push(hash)
            }
            Opcode::Index => {
                let index = self.pop();
                let left = self.pop();

                self.execute_index_expression(left, index)
            }
            Opcode::Call => {
                let num_args = ins[usize::try_from(ip + 1).unwrap()] as usize;
                self.current_frame_mut().ip += 1;

                self.execute_call(num_args)
            }
            Opcode::ReturnValue => {
                let return_value = self.pop();

                let frame = self.pop_frame();
                self.sp = usize::try_from(frame.bp).unwrap() - 1; // - 1 instead of popping the function obj

                self.push(return_value)
            }
            Opcode::Return => {
                let frame = self.pop_frame();
                self.sp = usize::try_from(frame.bp).unwrap() - 1; // - 1 instead of popping the function obj

                self.push(null_obj())
            }
            Opcode::SetLocal => {
                let local_ind = ins[usize::try_from(ip + 1).unwrap()] as usize;
                self.current_frame_mut().ip += 1;

                let frame_bp: usize = self.current_frame_mut().bp.try_into().unwrap();
                let local_var_val = self.pop();
                self.stack[frame_bp + local_ind] = local_var_val;
                Ok(())
            }
            Opcode::GetLocal => {
                let local_ind = ins[usize::try_from(ip + 1).unwrap()] as usize;
                self.current_frame_mut().ip += 1;

                let frame_bp: usize = self.current_frame_mut().bp.try_into().unwrap();
                self.push(self.stack[frame_bp + local_ind].clone())
            }
            Opcode::GetBuiltin => {
                let builtin_ind = ins[usize::try_from(ip + 1).unwrap()] as usize;
                self.current_frame_mut().ip += 1;

                let definition = BUILTINS
                    .get(builtin_ind)
                    .expect("index is always valid from compiler");
                // TODO: try to remove the clone by storing Rc<BuiltinObj>
                self.push(Rc::new(Object::Builtin(definition.1.clone())))
            }
            Opcode::Closure => {
                let const_ind = read_u16(&ins[ip as usize + 1..ip as usize + 3]) as usize;
                let num_free = ins[usize::try_from(ip + 3).unwrap()] as usize;
                self.current_frame_mut().ip += 3;
                self.push_closure(const_ind, num_free)
            }
            Opcode::GetFree => {
                let free_ind = ins[usize::try_from(ip + 1).unwrap()] as usize;
                self.current_frame_mut().ip += 1;
                let current_closure = &self.current_frame().cl;
                self.push(current_closure.free[free_ind].clone())
            }
            Opcode::CurrentClosure => {
                let current_closure = self.current_frame().cl.clone();
                self.push(Rc::new(Object::Closure(current_closure)))
            }
        }
    }

    pub fn is_running(&self) -> bool {
        self.current_frame().ip < (self.current_frame().cl.comp_fn.instructions.len() as i64 - 1)
    }

    pub fn run(&mut self) -> Result<(), String> {
        while self.is_running() {
            self.step()?;
        }
        Ok(())
    }

    fn execute_call(&mut self, num_args: usize) -> Result<(), String> {
        let callee = self.stack[self.sp - 1 - num_args].clone();
        match &*callee {
            Object::Closure(closure_obj) => self.call_closure(closure_obj.clone(), num_args),
            Object::Builtin(builtin_obj) => self.call_builtin(builtin_obj.clone(), num_args),
            _ => Err("calling non-closure and non-built-in".to_string()),
        }
    }

    fn call_closure(&mut self, cl: ClosureObj, num_args: usize) -> Result<(), String> {
        if num_args != cl.comp_fn.num_parameters {
            return Err(format!(
                "wrong number of arguments: want={}, got={num_args}",
                cl.comp_fn.num_parameters,
            ));
        }
        let num_locals = cl.comp_fn.num_locals;
        let frame = Frame::new(cl, usize::try_into(self.sp - num_args).unwrap());
        let next_sp: usize = frame.bp as usize + num_locals;
        self.push_frame(frame);
        self.sp = next_sp;
        Ok(())
    }

    fn call_builtin(&mut self, builtin_obj: BuiltinObj, num_args: usize) -> Result<(), String> {
        let args = &self.stack[self.sp - num_args..self.sp];
        let result = (builtin_obj.function)(args);
        self.sp = self.sp - num_args - 1;
        self.push(result)
    }

    pub fn push(&mut self, o: ObjRef) -> Result<(), String> {
        if self.sp >= STACKSIZE {
            Err("woops! stack overflow".to_string())
        } else {
            self.stack[self.sp] = o;
            self.sp += 1;
            Ok(())
        }
    }

    pub fn new_with_globals_store(bytecode: Bytecode, s: Vec<ObjRef>) -> Self {
        let mut vm = VM::new(bytecode);
        vm.globals = s;
        vm
    }

    fn build_array(&mut self, start_ind: usize, end_ind: usize) -> ObjRef {
        let mut elements = vec![null_obj(); end_ind - start_ind];
        let mut i = start_ind;
        while i < end_ind {
            elements[i - start_ind] = self.stack[i].clone();
            i += 1;
        }
        Object::new_array_var(elements)
    }

    fn build_hash(&mut self, start_ind: usize, end_ind: usize) -> Result<ObjRef, String> {
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

#[cfg(test)]
mod tests {
    use std::{any::Any, collections::HashMap};

    use crate::{
        ast::Node,
        compiler::Compiler,
        object::{Error, HashKey, Hashable, Null},
        parser::Parser,
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
            // >= and <= operators
            VmTestCase::new("1 >= 2", Box::new(false)),
            VmTestCase::new("2 >= 2", Box::new(true)),
            VmTestCase::new("3 >= 2", Box::new(true)),
            VmTestCase::new("1 <= 2", Box::new(true)),
            VmTestCase::new("2 <= 2", Box::new(true)),
            VmTestCase::new("3 <= 2", Box::new(false)),
            VmTestCase::new("(1 >= 1) == true", Box::new(true)),
            VmTestCase::new("(1 <= 1) == true", Box::new(true)),
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
    fn test_while_expressions() {
        let tests = vec![
            VmTestCase::new(
                "let x = 3; while (x > 0) { let x = x - 1; }; x",
                Box::new(0i64),
            ),
            VmTestCase::new(
                "let x = 0; while (x < 3) { let x = x + 1; }; x",
                Box::new(3i64),
            ),
            VmTestCase::new(
                "let x = 0; while (x <= 2) { let x = x + 1; }; x",
                Box::new(3i64),
            ),
            VmTestCase::new("while (2 < 1) { 10 }", Box::new(crate::object::Null)),
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
        for (i, VmTestCase { input, expected }) in tests.into_iter().enumerate() {
            let program = Parser::parse(input);
            let mut comp = Compiler::new();
            comp.compile(Node::Program(program))
                .unwrap_or_else(|e| panic!("compiler error: {e:?}"));
            println!("test {}:", i + 1);
            for (i, constant) in comp.bytecode().constants.iter().enumerate() {
                println!("CONSTANT {i} {constant:p} {}", constant.inspect());
                match &**constant {
                    Object::CompiledFunction(constant) => {
                        print!(" Instructions:\n{}", constant.instructions)
                    }
                    Object::Integer(constant) => println!(" Value: {}", constant.value),
                    _ => {}
                }
                println!()
            }
            let mut vm = VM::new(comp.bytecode());
            vm.run().unwrap_or_else(|e| panic!("vm error: {e}"));
            let stack_elm = vm.last_popped_stack_elem();
            test_expected_object(expected.as_ref(), &stack_elm);
        }
    }

    fn test_expected_object(expected: &dyn Any, actual: &ObjRef) {
        if let Some(expec) = expected.downcast_ref::<i64>() {
            test_integer_object(*expec, actual)
                .unwrap_or_else(|e| panic!("test_integer_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<bool>() {
            test_boolean_object(*expec, actual)
                .unwrap_or_else(|e| panic!("test_boolean_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<&str>() {
            test_string_object(expec, actual)
                .unwrap_or_else(|e| panic!("test_string_object failed: {e}"));
        } else if let Some(_expec) = expected.downcast_ref::<Null>() {
            test_null_object(actual).unwrap_or_else(|e| panic!("test_null_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<Vec<i64>>() {
            test_arr_ints_object(expec, actual)
                .unwrap_or_else(|e| panic!("test_arr_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<HashMap<HashKey, i64>>() {
            test_hash_ints_object(expec, actual)
                .unwrap_or_else(|e| panic!("test_arr_object failed: {e}"));
        } else if let Some(expec) = expected.downcast_ref::<Error>() {
            test_error_object(expec, actual)
                .unwrap_or_else(|e| panic!("test_arr_object failed: {e}"));
        } else {
            panic!("unknown dyn object: {:?}", expected);
        }
    }

    pub fn test_error_object(expected: &Error, actual: &ObjRef) -> Result<(), String> {
        if let Object::Err(err_obj) = &**actual {
            if err_obj.message != expected.message {
                Err(format!(
                    "wrong error message. expected={}, got={}",
                    expected.message, err_obj.message
                ))
            } else {
                Ok(())
            }
        } else {
            Err(format!("object is not Error: {actual:?}"))
        }
    }

    pub fn test_hash_ints_object(
        expected: &HashMap<HashKey, i64>,
        actual: &ObjRef,
    ) -> Result<(), String> {
        if let Object::Hash(result) = &**actual {
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

    pub fn test_arr_ints_object(expected: &[i64], actual: &ObjRef) -> Result<(), String> {
        if let Object::Arr(result) = &**actual {
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
    pub fn test_integer_object(expected: i64, actual: &ObjRef) -> Result<(), String> {
        if let Object::Integer(result) = &**actual {
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
    pub fn test_string_object(expected: &str, actual: &ObjRef) -> Result<(), String> {
        if let Object::String(result) = &**actual {
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

    pub fn test_boolean_object(expected: bool, actual: &ObjRef) -> Result<(), String> {
        if let Object::Boolean(result) = &**actual {
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

    pub fn test_null_object(actual: &ObjRef) -> Result<(), String> {
        if let Object::Null(_result) = &**actual {
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
            VmTestCase::new("[]", Box::new(vec![0_i64; 0])),
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
            VmTestCase::new(
                "{1: 1}[0]",
                Box::new(Error::new("key doesn't exist".to_string())),
            ),
            VmTestCase::new(
                "{}[0]",
                Box::new(Error::new("key doesn't exist".to_string())),
            ),
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
    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let tests = vec![
            VmTestCase::new(
                "let identity = fn(a) { a; };
identity(4);",
                Box::new(4i64),
            ),
            VmTestCase::new(
                "let sum = fn(a, b) { a + b; };
sum(1, 2);",
                Box::new(3i64),
            ),
            VmTestCase::new(
                "let sum = fn(a, b) {
let c = a + b;
c;
};
sum(1, 2);",
                Box::new(3i64),
            ),
            VmTestCase::new(
                "let sum = fn(a, b) {
let c = a + b;
c;
};
sum(1, 2) + sum(3, 4);",
                Box::new(10i64),
            ),
            VmTestCase::new(
                "let sum = fn(a, b) {
let c = a + b;
c;
};
let outer = fn() {
sum(1, 2) + sum(3, 4);
};
outer();",
                Box::new(10i64),
            ),
            VmTestCase::new(
                "let globalNum = 10;
let sum = fn(a, b) {
let c = a + b;
c + globalNum;
};
let outer = fn() {
sum(1, 2) + sum(3, 4) + globalNum;
};
outer() + globalNum;",
                Box::new(50i64),
            ),
        ];
        run_vm_tests(tests);
    }
    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        let tests = vec![
            VmTestCase::new(
                "fn() { 1; }(1);",
                Box::new("wrong number of arguments: want=0, got=1"),
            ),
            VmTestCase::new(
                "fn(a) { a; }();",
                Box::new("wrong number of arguments: want=1, got=0"),
            ),
            VmTestCase::new(
                "fn(a, b) { a + b; }(1);",
                Box::new("wrong number of arguments: want=2, got=1"),
            ),
        ];
        for VmTestCase { input, expected } in tests {
            let program = Parser::parse(input);

            let mut comp = Compiler::new();
            comp.compile(Node::Program(program))
                .unwrap_or_else(|e| panic!("compiler error: {e}"));

            let mut vm = VM::new(comp.bytecode());
            let err = vm.run();
            let Some(expected) = expected.downcast_ref::<&str>() else {
                panic!()
            };
            match err {
                Ok(_) => panic!("expected VM error but resulted in none."),
                Err(err_msg) => assert_eq!(
                    &err_msg, expected,
                    "wrong VM error: want={expected}, got={err_msg}"
                ),
            }
        }
        //run_vm_tests(tests);
    }
    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            VmTestCase::new(r#"len("")"#, Box::new(0i64)),
            VmTestCase::new(r#"len("four")"#, Box::new(4i64)),
            VmTestCase::new(r#"len("hello world")"#, Box::new(11i64)),
            VmTestCase::new(
                "len(1)",
                Box::new(Error::new(
                    "argument to `len` not supported, got INTEGER".to_string(),
                )),
            ),
            VmTestCase::new(
                r#"len("one", "two")"#,
                Box::new(Error::new(
                    "wrong number of arguments. got=2, want=1".to_string(),
                )),
            ),
            VmTestCase::new("len([1, 2, 3])", Box::new(3i64)),
            VmTestCase::new("len([])", Box::new(0i64)),
            VmTestCase::new(r#"puts("hello", "world!")"#, Box::new(Null)),
            VmTestCase::new("first([1, 2, 3])", Box::new(1i64)),
            VmTestCase::new("first([])", Box::new(Null)),
            VmTestCase::new(
                "first(1)",
                Box::new(Error::new(
                    "argument to `first` must be ARRAY, got INTEGER".to_string(),
                )),
            ),
            VmTestCase::new("last([1, 2, 3])", Box::new(3i64)),
            VmTestCase::new("last([])", Box::new(Null)),
            VmTestCase::new(
                "last(1)",
                Box::new(Error::new(
                    "argument to `last` must be ARRAY, got INTEGER".to_string(),
                )),
            ),
            VmTestCase::new("rest([1, 2, 3])", Box::new(vec![2i64, 3i64])),
            VmTestCase::new("push([], 1)", Box::new(vec![1i64])),
            VmTestCase::new(
                "push(1, 1)",
                Box::new(Error::new(
                    "argument to `push` must be ARRAY, got INTEGER".to_string(),
                )),
            ),
        ];

        run_vm_tests(tests);
    }
    #[test]
    fn test_closures() {
        let tests = vec![
            VmTestCase::new(
                "
let newClosure = fn(a) {
    fn() { a; };
};
let closure = newClosure(99);
closure();
",
                Box::new(99i64),
            ),
            VmTestCase::new(
                "
let newAdder = fn(a, b) {
fn(c) { a + b + c };
};
let adder = newAdder(1, 2);
adder(8);
",
                Box::new(11i64),
            ),
            VmTestCase::new(
                "
let newAdder = fn(a, b) {
let c = a + b;
fn(d) { c + d };
};
let adder = newAdder(1, 2);
adder(8);
",
                Box::new(11i64),
            ),
            VmTestCase::new(
                "
let newAdderOuter = fn(a, b) {
    let c = a + b;
    fn(d) {
        let e = d + c;
        fn(f) { e + f; };
    };
};
let newAdderInner = newAdderOuter(1, 2)
let adder = newAdderInner(3);
adder(8);
",
                Box::new(14i64),
            ),
            VmTestCase::new(
                "
let a = 1;
let newAdderOuter = fn(b) {
    fn(c) {
        fn(d) { a + b + c + d };
    };
};
let newAdderInner = newAdderOuter(2)
let adder = newAdderInner(3);
adder(8);
",
                Box::new(14i64),
            ),
            VmTestCase::new(
                "
let newClosure = fn(a, b) {
    let one = fn() { a; };
    let two = fn() { b; };
    fn() { one() + two(); };
};
let closure = newClosure(9, 90);
closure();
",
                Box::new(99i64),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_recursive_closures() {
        let tests = vec![
            VmTestCase::new(
                "
let countDown = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        countDown(x - 1);
    }
};
countDown(1);
",
                Box::new(0i64),
            ),
            VmTestCase::new(
                "
let countDown = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        countDown(x - 1);
    }
};
let wrapper = fn() {
    countDown(1);
};
wrapper();
",
                Box::new(0i64),
            ),
            VmTestCase::new(
                "
let wrapper = fn() {
    let countDown = fn(x) {
        if (x == 0) {
            return 0;
        } else {
            countDown(x - 1);
        }
    };
    countDown(1);
};
wrapper();
",
                Box::new(0i64),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_recursive_fibonacci() {
        let tests = vec![
            VmTestCase::new(
                "
let fibonacci = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
fibonacci(15);
",
                Box::new(610i64),
            ),
            VmTestCase::new(
                "
let countDown = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        countDown(x - 1);
    }
};
let wrapper = fn() {
    countDown(1);
};
wrapper();
",
                Box::new(0i64),
            ),
            VmTestCase::new(
                "
let wrapper = fn() {
    let countDown = fn(x) {
        if (x == 0) {
            return 0;
        } else {
            countDown(x - 1);
        }
    };
    countDown(1);
};
wrapper();
",
                Box::new(0i64),
            ),
        ];

        run_vm_tests(tests);
    }

    /// Update: the test case passes now and all the ranting below is outdated
    /// they are good notes though if I ever want to comback later and understand
    /// new globals override old ones;e.g. "let a=1;leta=2;" then old a is
    /// still stored but never referenced. try to make a test case for it;
    /// this is not only a problem of memory efficiency but also of behavious since
    /// now compiled functions reference old index values of the global. e.g.
    /// "let a =1; let my_fn(){a}; let a =2; my_fn()" -> returns 1 not 2 because a's
    /// index changed from 0 to something else.
    /// this is related to capturing free variables.
    /// note that this problem is complicated since it is only with global variables
    /// local variables that are on a higher scope should just be captured and saved so that when
    /// you call the closure inside another function that defined the variable a with the same name
    /// it should use the captured old value not the new one.
    #[test]
    fn test_global_variables_shouldnt_be_captures_as_free() {
        let tests = vec![
            VmTestCase::new(
                "
let a = 1;
let my_fn = fn(){a};
let a = 2;
my_fn()
",
                Box::new(2i64),
            ),
            VmTestCase::new(
                "
let my_fn = fn(){
    let a = 1;
    fn(){a}
};
let another_fn = my_fn();
let a = 2;
another_fn()
",
                Box::new(1i64),
            ),
            VmTestCase::new(
                "
let global = 55;
let my_fn = fn() {
    let a = 66;
    fn() {
        let b = 77;
        fn() {
            let c = 88;
            global + a + b + c;
        }
    }
}
let global = 1;
let a = 0; # doesn't matter
let b = 0; # doesn't matter
let c = 0; # doesn't matter
my_fn()()()
",
                Box::new(1_i64 + 66 + 77 + 88),
            ),
            VmTestCase::new(
                "
let a = 1;
let my_fn = fn() { let a = 2; a};
let a = 3;
my_fn()
",
                Box::new(2i64),
            ),
        ];

        run_vm_tests(tests);
    }
}
