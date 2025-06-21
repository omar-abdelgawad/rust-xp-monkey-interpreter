use crate::ast::{
    self, Expression, ExpressionStatement, IfExpression, IntegerLiteral, Node, Program, Statement,
};
use crate::object::environment::Environment;
use crate::object::{
    native_bool_to_boolean_object, Error, Function, ObjectTrait, ObjectType, ReturnValue,
};
use crate::object::{Boolean, Integer, Null, Object};
use crate::object::{FALSE, NULL, TRUE};

use std::cell::RefCell;
use std::rc::Rc;

pub fn eval(node: Node, env: &mut Environment) -> Object {
    use Expression as Exp;
    use Statement as St;
    match node {
        Node::Program(prog) => eval_program(prog.statements, env),
        Node::Statement(stmt) => match stmt {
            St::Expression(exp_stmt) => eval(Node::Expression(*exp_stmt.expression), env),
            St::Block(block_stmt) => eval_block_statement(block_stmt.statements, env),
            St::Return(ret_stmt) => {
                let val = eval(Node::Expression(*ret_stmt.return_value), env);
                if is_error(&val) {
                    return val;
                }
                Object::new_ret_var(val)
            }
            St::Let(let_stmt) => {
                let tmp_name_value = let_stmt.name_value();
                let val = eval(Node::Expression(*let_stmt.value), env);
                if is_error(&val) {
                    return val;
                }
                env.set(tmp_name_value, val)
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
                let right = eval(Node::Expression(*prefix_exp.right), env);
                if is_error(&right) {
                    return right;
                }
                eval_prefix_expression(&prefix_exp.operator, right)
            }
            Exp::Infix(infix_exp) => {
                let left = eval(Node::Expression(*infix_exp.left), env);
                if is_error(&left) {
                    return left;
                }
                let right = eval(Node::Expression(*infix_exp.right), env);
                if is_error(&right) {
                    return right;
                }
                eval_infix_expression(&infix_exp.operator, left, right)
            }
            Exp::If(if_exp) => eval_if_exp(if_exp, env),
            Exp::Identifier(ident) => eval_identifier(ident, env),
            Exp::Function(fun_exp) => {
                let params = fun_exp.parameters;
                let body = *fun_exp.body.unwrap();
                Object::Func(Function::new(params, body, env.clone()))
            }
            Exp::Call(call_exp) => {
                let func = eval(Node::Expression(*call_exp.function), env);
                if is_error(&func) {
                    return func;
                }
                let mut args = eval_expressions(call_exp.arguments, env);
                if args.len() == 1 && is_error(&args[0]) {
                    return args.remove(0);
                }
                apply_function(&func, &args)
            }
            unknown_exp_node => {
                unreachable!(
                    "you have reached an unhandled Expression node: {:?}",
                    unknown_exp_node
                )
            }
        },
    }
}
fn eval_program(stmts: Vec<Statement>, env: &mut Environment) -> Object {
    let mut result = NULL;
    for statement in stmts {
        result = eval(Node::Statement(statement), env);
        match result {
            Object::Ret(ret_obj) => return *ret_obj.value,
            Object::Err(ref err_obj) => return result,
            _ => {}
        }
    }
    result
}

fn eval_block_statement(stmts: Vec<Statement>, env: &mut Environment) -> Object {
    let mut result = NULL;
    for statement in stmts {
        result = eval(Node::Statement(statement), env);
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
        // I still hate this default operator shit
        (op) => {
            return new_error(format!(
                "unknown operator: {} {} {}",
                left.r#type(),
                op,
                right.r#type()
            ))
        }
    }
}

fn eval_if_exp(ie: IfExpression, env: &mut Environment) -> Object {
    let cond = eval(Node::Expression(*ie.condition), env);
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
fn eval_identifier(ident: ast::Identifier, env: &mut Environment) -> Object {
    let val = env.get(&ident.value);
    match val {
        Some(obj) => obj,
        None => new_error(format!("identifier not found: {}", ident.value)),
    }
}

fn eval_expressions(exps: Vec<Box<Expression>>, env: &mut Environment) -> Vec<Object> {
    let mut res = Vec::new();
    for e in exps {
        let evaluated = eval(Node::Expression(*e), env);
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        res.push(evaluated);
    }
    res
}

// TODO: the following 3 functions below are not well thought and I
// just used clone everytime I saw an error. I don't care now about
// nothing except compiling but probably the types are all wrong
// from the Option<Object> to the not shared Environment in
// fn_obj of type Object::Fun(Function)
fn apply_function(fn_obj: &Object, args: &[Object]) -> Object {
    let Object::Func(fn_obj) = fn_obj else {
        return new_error(format!("not a function: {}", fn_obj.r#type()));
    };
    let mut extended_env = extend_function_env(fn_obj, args);
    let evaluated = eval(
        Node::Statement(Statement::Block(fn_obj.body.clone())),
        &mut extended_env,
    );

    unwrap_return_value(evaluated)
}

fn extend_function_env(fn_obj: &Function, args: &[Object]) -> Environment {
    // FIX: this clones a snapshot of enclosing environment but
    // it should take a reference instead?? so maybe make
    // fn_obj already have an Rc<RefCell<Environment>>
    let mut env = Environment::new_enclosed_environment(Rc::new(RefCell::new(fn_obj.env.clone())));
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
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
        eval(Node::Program(program), &mut env)
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

    fn test_null_object(obj: &Object) -> bool {
        match obj {
            Object::Null(_) => true,
            _ => {
                eprintln!("object is not NULL. got={:?} ({:?})", obj, obj);
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
}
