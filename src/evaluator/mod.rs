mod builtins;
use builtins::builtins;

use crate::ast::{
    self, Expression, ExpressionStatement, HashLiteral, IfExpression, IntegerLiteral, Node,
    Program, Statement, WhileExpression,
};
use crate::object::environment::Environment;
use crate::object::{
    native_bool_to_boolean_object, Array, Error, Function, HashObj, HashPair, Hashable,
    ObjectTrait, ObjectType, ReturnValue, StringObj,
};
use crate::object::{Boolean, Integer, Null, Object};
use crate::object::{FALSE, NULL, TRUE};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// TODO: in order to return Objects from the environment without cloning it we should
// maybe return Cow<'a,Object> or Rc<RefCell<Object>> but that changes all function signatures
// and some of the logic as well and it seems ugly unfortunately.
pub fn eval(node: Node, env: Rc<RefCell<Environment>>) -> Object {
    use Expression as Exp;
    use Statement as St;
    match node {
        Node::Program(prog) => eval_program(prog.statements, Rc::clone(&env)),
        Node::Statement(stmt) => match stmt {
            St::Expression(exp_stmt) => {
                eval(Node::Expression(*exp_stmt.expression), Rc::clone(&env))
            }
            St::Block(block_stmt) => eval_block_statement(block_stmt.statements, Rc::clone(&env)),
            St::Return(ret_stmt) => {
                let val = eval(Node::Expression(*ret_stmt.return_value), Rc::clone(&env));
                if is_error(&val) {
                    return val;
                }
                Object::new_ret_var(val)
            }
            St::Let(let_stmt) => {
                let tmp_name_value = let_stmt.name_value();
                let val = eval(Node::Expression(*let_stmt.value), Rc::clone(&env));
                if is_error(&val) {
                    return val;
                }
                env.borrow_mut().set(tmp_name_value, val)
            }
            unknown_st_node => {
                unreachable!(
                    "you have reached an unhandled Statement node: {:?}",
                    unknown_st_node
                )
            }
        },
        Node::Expression(exp) => match exp {
            Exp::Integer(int_lit) => Object::Integer(Integer::new(int_lit.value)),
            Exp::Boolean(bool_lit) => native_bool_to_boolean_object(bool_lit.value),
            Exp::Prefix(prefix_exp) => {
                let right = eval(Node::Expression(*prefix_exp.right), Rc::clone(&env));
                if is_error(&right) {
                    return right;
                }
                eval_prefix_expression(&prefix_exp.operator, right)
            }
            Exp::Infix(infix_exp) => {
                let left = eval(Node::Expression(*infix_exp.left), Rc::clone(&env));
                if is_error(&left) {
                    return left;
                }
                let right = eval(Node::Expression(*infix_exp.right), Rc::clone(&env));
                if is_error(&right) {
                    return right;
                }
                eval_infix_expression(&infix_exp.operator, left, right)
            }
            Exp::If(if_exp) => eval_if_exp(if_exp, Rc::clone(&env)),
            Exp::Identifier(ident) => eval_identifier(ident, Rc::clone(&env)),
            Exp::Function(fun_exp) => {
                let params = fun_exp.parameters;
                let body = *fun_exp.body.unwrap();
                Object::Func(Function::new(params, body, Rc::clone(&env)))
            }
            Exp::Call(call_exp) => {
                let func = eval(Node::Expression(*call_exp.function), Rc::clone(&env));
                if is_error(&func) {
                    return func;
                }
                let mut args = eval_expressions(call_exp.arguments, Rc::clone(&env));
                if args.len() == 1 && is_error(&args[0]) {
                    return args.remove(0);
                }
                apply_function(&func, &args)
            }
            Exp::Str(str_lit) => Object::String(StringObj::new(str_lit.value)),
            Exp::Arr(arr_lit) => {
                let mut elems = eval_expressions(arr_lit.elements, Rc::clone(&env));
                if elems.len() == 1 && is_error(&elems[0]) {
                    return elems.remove(0);
                }
                Object::Arr(Array::new(elems))
            }
            Exp::Ind(ind_exp) => {
                let left = eval(Node::Expression(*ind_exp.left), Rc::clone(&env));
                if is_error(&left) {
                    return left;
                }
                let ind = eval(Node::Expression(*ind_exp.index), Rc::clone(&env));
                if is_error(&ind) {
                    return ind;
                }
                eval_index_expression(left, ind)
            }
            Exp::Hash(hash_lit) => eval_hash_litral(hash_lit, Rc::clone(&env)),
            Exp::While(while_exp) => eval_while_exp(while_exp, Rc::clone(&env)),
            unknown_exp_node => {
                unreachable!(
                    "you have reached an unhandled Expression node: {:?}",
                    unknown_exp_node
                )
            }
        },
    }
}
fn eval_program(stmts: Vec<Statement>, env: Rc<RefCell<Environment>>) -> Object {
    let mut result = NULL;
    for statement in stmts {
        result = eval(Node::Statement(statement), Rc::clone(&env));
        match result {
            Object::Ret(ret_obj) => return *ret_obj.value,
            Object::Err(ref err_obj) => return result,
            _ => {}
        }
    }
    result
}

fn eval_block_statement(stmts: Vec<Statement>, env: Rc<RefCell<Environment>>) -> Object {
    let mut result = NULL;
    for statement in stmts {
        result = eval(Node::Statement(statement), Rc::clone(&env));
        match result {
            Object::Ret(_) | Object::Err(_) => return result,
            _ => {}
        }
    }
    result
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_exprssion(right),
        "+" => eval_plus_prefix_operator_exprssion(right),
        _ => new_error(format!("unknown operator: {}{}", operator, right.r#type(),)),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        // TODO: remove this false or make it panic?
        // this default means that all objects are "truthy"
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_exprssion(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::new_int_var(-integer.value),
        // I REALLY HATE THIS
        _ => new_error(format!("unknown operator: -{}", right.r#type())),
    }
}

fn eval_plus_prefix_operator_exprssion(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::new_int_var(integer.value),
        // I REALLY HATE THIS
        _ => new_error(format!("unknown operator: -{}", right.r#type())),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    use Object as Obj;
    match (operator, left, right) {
        ("==", Obj::Boolean(left), Obj::Boolean(right)) => {
            native_bool_to_boolean_object(left == right)
        }
        ("!=", Obj::Boolean(left), Obj::Boolean(right)) => {
            native_bool_to_boolean_object(left != right)
        }
        (_, Obj::Integer(left), Obj::Integer(right)) => {
            eval_integer_infix_expression(operator, left, right)
        }
        (_, Obj::String(left), right) => eval_string_infix_expression(operator, left, right),
        (op, l, r) if l.r#type() != r.r#type() => new_error(format!(
            "type mismatch: {} {} {}",
            l.r#type(),
            operator,
            r.r#type()
        )),
        (op, l, r) => new_error(format!(
            "unknown operator: {} {} {}",
            l.r#type(),
            op,
            r.r#type(),
        )),
    }
}

fn eval_string_infix_expression(operator: &str, left: StringObj, right: Object) -> Object {
    match (operator, right) {
        ("+", Object::String(right)) => {
            let new_str = format!("{}{}", left.value, right.value);
            Object::String(StringObj::new(new_str))
        }
        ("+", Object::Integer(right)) => {
            let new_str = format!("{}{}", left.value, right.value);
            Object::String(StringObj::new(new_str))
        }
        // ("+", Object::Boolean(right)) => {
        //     let new_str = format!("{}{}", left.value, right.value);
        //     Object::String(StringObj::new(new_str))
        // }
        (op, right) => new_error(format!(
            "unknown operator: {} {} {}",
            left.r#type(),
            op,
            right.r#type()
        )),
    }
}

fn eval_integer_infix_expression(operator: &str, left: Integer, right: Integer) -> Object {
    let left_val = left.value;
    let right_val = right.value;
    match operator {
        "+" => Object::new_int_var(left_val + right_val),
        "-" => Object::new_int_var(left_val - right_val),
        "*" => Object::new_int_var(left_val * right_val),
        "/" => Object::new_int_var(left_val / right_val),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        ">=" => native_bool_to_boolean_object(left_val >= right_val),
        "<=" => native_bool_to_boolean_object(left_val <= right_val),
        // I still hate this default operator shit
        (op) => {
            return new_error(format!(
                "unknown operator: {} {} {}",
                left.r#type(),
                op,
                right.r#type()
            ));
        }
    }
}

fn eval_if_exp(ie: IfExpression, env: Rc<RefCell<Environment>>) -> Object {
    let cond = eval(Node::Expression(*ie.condition), Rc::clone(&env));
    if is_error(&cond) {
        return cond;
    }
    if is_truthy(cond) {
        return eval(Node::Statement(Statement::Block(*ie.consequence)), env);
    } else if ie.alternative.is_some() {
        return eval(
            Node::Statement(Statement::Block(*ie.alternative.unwrap())),
            env,
        );
    } else {
        NULL
    }
}

fn eval_while_exp(ie: WhileExpression, env: Rc<RefCell<Environment>>) -> Object {
    let mut cond = eval(Node::Expression(*ie.condition.clone()), Rc::clone(&env));
    if is_error(&cond) {
        return cond;
    }
    while is_truthy(cond) {
        let _ = eval(
            Node::Statement(Statement::Block(*ie.loop_body.clone())),
            env.clone(),
        );
        cond = eval(Node::Expression(*ie.condition.clone()), Rc::clone(&env));
        if is_error(&cond) {
            return cond;
        }
    }
    NULL
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

fn new_error(formt: String) -> Object {
    Object::Err(Error::new(formt))
}

fn is_error(obj: &Object) -> bool {
    obj.r#type() == ObjectType::ERROR_OBJ
}
fn eval_identifier(ident: ast::Identifier, env: Rc<RefCell<Environment>>) -> Object {
    let val = env.borrow().get(&ident.value);
    if let Some(obj) = val {
        return obj;
    }

    // Use builtins (which now handle WebAssembly vs native compilation internally)
    let val = builtins(&ident.value);
    if let Some(built_obj) = val {
        return Object::Builtin(built_obj);
    }

    new_error(format!("identifier not found: {}", ident.value))
}

fn eval_expressions(exps: Vec<Expression>, env: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut res = Vec::new();
    for e in exps {
        let evaluated = eval(Node::Expression(e), Rc::clone(&env));
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        res.push(evaluated);
    }
    res
}

fn apply_function(fn_obj: &Object, args: &[Object]) -> Object {
    match fn_obj {
        Object::Func(fn_obj) => {
            if fn_obj.parameters.len() != args.len() {
                return new_error(format!(
                    "non matching number of arguments. function takes {}, got={}.",
                    fn_obj.parameters.len(),
                    args.len()
                ));
            }
            let mut extended_env = extend_function_env(fn_obj, args);
            let evaluated = eval(
                Node::Statement(Statement::Block(fn_obj.body.clone())),
                Rc::new(RefCell::new(extended_env)),
            );

            unwrap_return_value(evaluated)
        }
        Object::Builtin(builtin_fn) => (builtin_fn.function)(args),
        _ => new_error(format!("not a function: {}", fn_obj.r#type())),
    }
}

fn extend_function_env(fn_obj: &Function, args: &[Object]) -> Environment {
    // FIX: this clones a snapshot of enclosing environment but
    // it should take a reference instead?? so maybe make
    // fn_obj already have an Rc<RefCell<Environment>>
    let mut env = Environment::new_enclosed_environment(Rc::clone(&fn_obj.env));
    for (param_idx, param) in fn_obj.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[param_idx].clone());
    }
    env
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::Ret(ret_obj) = obj {
        return *ret_obj.value;
    }
    return obj;
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left, index) {
        (Object::Arr(arr), Object::Integer(ind)) => eval_array_index_expresssion(arr, ind),
        (Object::Hash(hash_obj), right_obj) => eval_hash_index_expression(&hash_obj, right_obj),
        (left, _) => new_error(format!("index operator not supported: {}", left.r#type())),
    }
}

fn eval_array_index_expresssion(arr: Array, index: Integer) -> Object {
    let idx = if index.value < 0 {
        (arr.elements.len() as i64 + index.value) as usize
    } else {
        index.value as usize
    };
    if arr.elements.len() == 0
        || idx > arr.elements.len() - 1
        || index.value < -(arr.elements.len() as i64)
    {
        return new_error(format!(
            "array index out of bounds: the len is {} but the index is {}",
            arr.elements.len(),
            index.value
        ));
    }
    // FIX: this should be a reference but for now eval doesn't
    // return references to Object so we are stuck cloning
    arr.elements[idx].clone()
}

fn eval_hash_index_expression(hash: &HashObj, index: Object) -> Object {
    match index {
        Object::Boolean(_) | Object::Integer(_) | Object::String(_) => {}
        _ => return new_error(format!("unusable as hash key: {}", index.r#type())),
    }
    hash.get(&index)
}

fn eval_hash_litral(node: HashLiteral, env: Rc<RefCell<Environment>>) -> Object {
    let mut pairs = HashMap::new();
    for (key_node, value_node) in node.pairs {
        let key = eval(Node::Expression(key_node), Rc::clone(&env));
        if is_error(&key) {
            return key;
        }
        match key {
            Object::Boolean(_) | Object::Integer(_) | Object::String(_) => {}
            _ => return new_error(format!("unusable as hash key: {}", key.r#type())),
        }

        let value = eval(Node::Expression(value_node), Rc::clone(&env));
        if is_error(&value) {
            return value;
        }
        let hashed = key.hash_key();
        pairs.insert(hashed, HashPair::new(key, value));
    }
    Object::Hash(HashObj::new(pairs))
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;
    use crate::lexer::Lexer;
    use crate::object::HashKey;
    use crate::object::Hashable;
    use crate::object::Integer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5i64),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("+4", 4),
            ("+-5", -5),
            ("-+6", -6),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert!(test_integer_object(&evaluated, expected));
        }
    }
    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();
        eval(Node::Program(program), Rc::new(RefCell::new(env)))
    }
    fn test_integer_object(obj: &Object, expected: i64) -> bool {
        if let Object::Integer(result) = obj {
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
            eprintln!("object is not Integer. got={:?} ({:?})", obj, obj);
            false
        }
    }
    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("1 >= 2", false),
            ("2 >= 2", true),
            ("1 <= 2", true),
            ("2 <= 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert!(test_boolean_object(&evaluated, expected));
        }
    }
    fn test_boolean_object(obj: &Object, expected: bool) -> bool {
        let Object::Boolean(result) = obj else {
            eprintln!("object is not Boolean. got={:?} ({:?})", obj, obj);
            return false;
        };
        if result.value != expected {
            eprintln!(
                "object has wrong value. got={}, want{}",
                result.value, expected
            );
            false
        } else {
            true
        }
    }
    // TODO: FIX copying later.
    #[should_panic]
    #[test]
    fn test_object_identity() {
        let obj1 = native_bool_to_boolean_object(true);
        let obj2 = native_bool_to_boolean_object(true);

        // Pointers will be different because `Object` is Copy, so we get new values each time.
        let addr1 = &obj1 as *const _;
        let addr2 = &obj2 as *const _;

        println!("obj1 addr: {:p}", addr1);
        println!("obj2 addr: {:p}", addr2);

        assert_eq!(addr1, addr2, "Objects should have different addresses");
    }
    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert!(test_boolean_object(&evaluated, expected));
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests: Vec<(&str, Option<i64>)> = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Some(expected) => assert!(test_integer_object(&evaluated, expected)),
                None => assert!(test_null_object(&evaluated)),
            };
        }
    }

    #[test]
    fn test_while_expressions() {
        let tests: Vec<(&str, Option<i64>)> = vec![
            ("let x = true; while (x) { let x = false; }; 10", Some(10)),
            ("let x = 3; while (x>0) { let x = x-1; }; x", Some(0)),
            ("let x = 0; while (x < 3) { let x = x+1 }; x", Some(3)),
            ("while (2 < 1) { 10 }", None),
            ("let x = true; while (x) { let x = false; };", None),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Some(expected) => assert!(test_integer_object(&evaluated, expected)),
                None => assert!(test_null_object(&evaluated)),
            };
        }
    }

    fn test_null_object(obj: &Object) -> bool {
        match obj {
            Object::Null(_) => true,
            _ => {
                eprintln!("object is not NULL. got={:?} ({:?})", obj, obj);
                false
            }
        }
    }

    fn test_err_object(obj: &Object) -> bool {
        match obj {
            Object::Err(_) => true,
            _ => {
                eprintln!("object is not Err. got={:?} ({:?})", obj, obj);
                false
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;  
                    }
                    return 1;
                }",
                10,
            ),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert!(test_integer_object(&evaluated, expected))
        }
    }
    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) {\
                    if (10 > 1) {\
                        return true + false;\
                    }\
                    return 1;\
                }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
            ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING"),
            (
                "{\"name\": \"Monkey\"}[fn(x) {x}];",
                "unusable as hash key: FUNCTION",
            ),
            (
                "[1, 2, 3][3]",
                "array index out of bounds: the len is 3 but the index is 3",
            ),
            (
                "[1, 2, 3][-4]",
                "array index out of bounds: the len is 3 but the index is -4",
            ),
        ];
        let mut errors = vec![];
        for (input, expected_message) in tests {
            let evaluated = test_eval(input);
            let Object::Err(err_obj) = evaluated else {
                errors.push((
                    input,
                    format!("no error object returned. got={:?}", evaluated),
                ));
                continue;
            };
            assert_eq!(
                err_obj.message, expected_message,
                "wrong error message. expected={}, got={}",
                expected_message, err_obj.message
            )
        }
        if !errors.is_empty() {
            panic!("Tests failed with errors:\n{:?}", errors)
        }
    }
    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            assert!(test_integer_object(&test_eval(input), expected));
        }
    }
    #[test]
    fn test_function_object() {
        let input = "fn(x) {x+2;};";
        let evaluated = test_eval(input);
        //println!("{:?}", evaluated);
        let Object::Func(func_obj) = evaluated else {
            panic!(
                "object is not Function. got={:?} ({:?})",
                evaluated, evaluated
            )
        };
        if func_obj.parameters.len() != 1 {
            panic!(
                "function has wrong parameters. Parameters ={:?}",
                func_obj.parameters
            )
        }
        if func_obj.parameters[0].to_string() != "x" {
            panic!("parameter is not 'x'. got={}", func_obj.parameters[0])
        }
        let expected_body = "(x + 2)";
        if func_obj.body.to_string() != expected_body {
            panic!(
                "body is not {}. got={}",
                expected_body,
                func_obj.body.to_string()
            )
        }
    }
    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];
        for (input, expected) in tests {
            assert!(test_integer_object(&test_eval(input), expected));
        }
    }

    #[test]
    fn test_closures() {
        let input = "let newAdder = fn(x) {
fn(y) {x + y};
};

let addTwo = newAdder(2);
addTwo(2);";
        assert!(test_integer_object(&test_eval(input), 4));
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";

        let evaluated = test_eval(input);
        let Object::String(str_obj) = evaluated else {
            panic!("object is not String. got={:?}", evaluated);
        };
        assert_eq!(
            str_obj.value, "Hello World!",
            "String has wrong value. got={}",
            str_obj.value
        );
    }

    #[test]
    fn test_string_concat() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let evaluated = test_eval(input);
        let Object::String(str_obj) = evaluated else {
            panic!("object is not String. got={:?}", evaluated);
        };
        assert_eq!(
            str_obj.value, "Hello World!",
            "String has wrong value. got={}",
            str_obj.value
        );
    }

    #[test]
    fn test_builtin_functions() {
        enum Expec<'a> {
            Val(i64),
            Mess(&'a str),
            NULL,
        }
        let tests = vec![
            // len function
            ("len(\"\")", Expec::Val(0)),
            ("len(\"four\")", Expec::Val(4)),
            ("len(\"hello world\")", Expec::Val(11)),
            (
                "len(1)",
                Expec::Mess("argument to \"len\" not supported, got INTEGER"),
            ),
            (
                "len(\"one\", \"two\")",
                Expec::Mess("wrong number of arguments. got=2, want=1"),
            ),
            ("len([0,1,2])", Expec::Val(3)),
            ("len({})", Expec::Val(0)),
            ("len({\"hello\":true})", Expec::Val(1)),
            ("len({1:true, 34:false})", Expec::Val(2)),
            // first function
            ("first([])", Expec::NULL),
            ("first([1,2])", Expec::Val(1)),
            ("first(first([[2,4],3]))", Expec::Val(2)),
            // last function
            ("last([])", Expec::NULL),
            ("last([1,2,3])", Expec::Val(3)),
            ("last(last([1,2,[3,4]]))", Expec::Val(4)),
            // rest function
            ("rest([])", Expec::NULL),
            ("first(rest([1,2,3,4]))", Expec::Val(2)),
            ("first(rest(rest([1,2,3,4])))", Expec::Val(3)),
            ("first(rest(rest(rest([1,2,3,4]))))", Expec::Val(4)),
            ("first(rest(rest(rest(rest([1,2,3,4]))))", Expec::NULL),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Expec::NULL => assert!(test_null_object(&evaluated)),
                Expec::Val(val) => assert!(test_integer_object(&evaluated, val)),
                Expec::Mess(mess) => {
                    let Object::Err(err_obj) = evaluated else {
                        panic!("object is not Error. got={:?}", evaluated);
                    };
                    assert_eq!(
                        err_obj.message, mess,
                        "wrong error message. expected={}, got={}",
                        mess, err_obj.message
                    );
                }
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        let Object::Arr(arr_obj) = evaluated else {
            panic!("object is not Array. got={:?}", evaluated);
        };
        assert_eq!(
            arr_obj.elements.len(),
            3,
            "array has wrong num of elements. got={}",
            arr_obj.elements.len()
        );
        assert!(test_integer_object(&arr_obj.elements[0], 1));
        assert!(test_integer_object(&arr_obj.elements[1], 4));
        assert!(test_integer_object(&arr_obj.elements[2], 6));
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Some(1)),
            ("[1, 2, 3][1]", Some(2)),
            ("[1, 2, 3][2]", Some(3)),
            ("let i = 0; [1][i];", Some(1)),
            ("[1, 2, 3][1 + 1];", Some(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Some(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Some(2),
            ),
            ("[1, 2, 3][-1]", Some(3)),
            ("[1, 2, 3][-3]", Some(1)),
            ("[1, 2, 3][-4]", None),
            ("[1, 2, 3][3]", None),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert!(
                match expected {
                    Some(x) => test_integer_object(&evaluated, x as i64),
                    None => test_err_object(&evaluated),
                },
                "test: {} failed",
                input
            );
        }
    }

    #[test]
    fn test_recursion() {
        let input = "let fibonacci = fn(x) {
                    if (x == 0) { 0 } 
                    else { if (x == 1) { 1 } 
                           else {fibonacci(x - 1) + fibonacci(x - 2);}
                         }
                    };
                    fibonacci(4)"; // fib(4) = 3;
        let evaluated = test_eval(input);
        assert!(test_integer_object(&evaluated, 3));
    }

    #[test]
    fn test_map() {
        let input = "let map = fn(arr, f) {
                let iter = fn(arr, accumulated) 
                {
                    if (len(arr) == 0) {
                        accumulated
                    } else {
                        iter(rest(arr), push(accumulated, f(first(arr))));
                    }
                };
                iter(arr, []);
            };
            let a = [1,2,3,4];
            let double = fn(x) { x * 2 };
map(a, double);";
        let evaluated = test_eval(input);
        let Object::Arr(arr_obj) = evaluated else {
            panic!("object is not Array. got={:?}", evaluated);
        };
        assert_eq!(
            arr_obj.elements.len(),
            4,
            "array has wrong num of elements. got={}",
            arr_obj.elements.len()
        );
        assert!(test_integer_object(&arr_obj.elements[0], 2));
        assert!(test_integer_object(&arr_obj.elements[1], 4));
        assert!(test_integer_object(&arr_obj.elements[2], 6));
        assert!(test_integer_object(&arr_obj.elements[3], 8));
    }

    #[test]
    fn test_reduce() {
        let input = "let reduce = fn(arr, initial, f) 
    {
        let iter = fn(arr, result) {
            if (len(arr) == 0) {
                result
            } else {
                iter(rest(arr), f(result, first(arr)));
            }
        };
        iter(arr, initial);
    };
    let sum = fn(arr) {
        reduce(arr, 0, fn(initial, el) { initial + el });
    };
    sum([1, 2, 3, 4, 5]);
";
        let evaluated = test_eval(input);
        assert!(test_integer_object(&evaluated, 15));
    }

    #[test]
    fn test_hash_literals() {
        let input = "let two = \"two\";
            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6,
            }";
        let evaluated = test_eval(input);
        let Object::Hash(hash_obj) = evaluated else {
            panic!("object is not Hash. got={:?}", evaluated);
        };
        let mut expected: HashMap<HashKey, i64> = HashMap::new();
        expected.insert(Object::new_str_var("one").hash_key(), 1);
        expected.insert(Object::new_str_var("two").hash_key(), 2);
        expected.insert(Object::new_str_var("three").hash_key(), 3);
        expected.insert(Object::new_int_var(4).hash_key(), 4);
        expected.insert(TRUE.hash_key(), 5);
        expected.insert(FALSE.hash_key(), 6);

        assert_eq!(
            hash_obj.pairs.len(),
            expected.len(),
            "Hash has wrong num of pairs. got={}",
            hash_obj.pairs.len()
        );
        for (expected_key, expected_val) in expected {
            let pair = hash_obj.pairs.get(&expected_key);
            match pair {
                None => panic!("no pair for given key in pairs"),
                Some(hash_pair) => assert!(test_integer_object(&hash_pair.val, expected_val)),
            };
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"]", Some(5)),
            ("{\"foo\": 5}[\"bar\"]", None),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Some(5)),
            ("{}[\"foo\"]", None),
            ("{5: 5}[5]", Some(5)),
            ("{true: 5}[true]", Some(5)),
            ("{false: 5}[false]", Some(5)),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert!(match expected {
                None => test_null_object(&evaluated),
                Some(i) => test_integer_object(&evaluated, i),
            });
        }
    }
}
