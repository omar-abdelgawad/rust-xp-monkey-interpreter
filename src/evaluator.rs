use crate::ast::{
    Expression, ExpressionStatement, IfExpression, IntegerLiteral, Node, Program, Statement,
};
use crate::object::native_bool_to_boolean_object;
use crate::object::{Boolean, Integer, Null, Object};
use crate::object::{FALSE, NULL, TRUE};

pub fn eval(node: Node) -> Option<Object> {
    use Expression as Exp;
    use Statement as St;
    match node {
        Node::Program(prog) => eval_statements(prog.statements),
        Node::Statement(stmt) => match stmt {
            St::Expression(exp_stmt) => eval(Node::Expression(*exp_stmt.expression)),
            St::Block(block_stmt) => eval_statements(block_stmt.statements),
            _ => None,
        },
        Node::Expression(exp) => match exp {
            Exp::Integer(int_lit) => Some(Object::Integer(Integer::new(int_lit.value))),
            Exp::Boolean(bool_lit) => Some(native_bool_to_boolean_object(bool_lit.value)),
            Exp::Prefix(prefix_exp) => {
                let right = eval(Node::Expression(*prefix_exp.right));
                Some(eval_prefix_expression(&prefix_exp.operator, right))
            }
            Exp::Infix(infix_exp) => {
                let left = eval(Node::Expression(*infix_exp.left));
                let right = eval(Node::Expression(*infix_exp.right));
                Some(eval_infix_expression(&infix_exp.operator, left, right))
            }
            Exp::If(if_exp) => eval_if_exp(if_exp),
            _ => None,
        },
    }
}
fn eval_statements(stmts: Vec<Statement>) -> Option<Object> {
    let mut result = None;
    for statement in stmts {
        result = eval(Node::Statement(statement));
    }
    result
}

fn eval_prefix_expression(operator: &str, right: Option<Object>) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_exprssion(right),
        _ => NULL,
    }
}

fn eval_bang_operator_expression(right: Option<Object>) -> Object {
    let Some(right) = right else {
        panic!("ERROR: deal with null values on the right of bang exprssions YOU MR!")
    };
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        // TODO: remove this false or make it panic?
        // this default means that all objects are "truthy"
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_exprssion(right: Option<Object>) -> Object {
    let Some(right) = right else {
        panic!("ERROR: deal with null values on the right of minus exprssions YOU MR!")
    };
    match right {
        Object::Integer(integer) => Object::new_int_var(-integer.value),
        // I REALLY HATE THIS
        _ => NULL,
    }
}

fn eval_infix_expression(operator: &str, left: Option<Object>, right: Option<Object>) -> Object {
    let Some(left) = left else {
        panic!("ERROR: deal with null values on the left of infix exprssions YOU MR!")
    };
    let Some(right) = right else {
        panic!("ERROR: deal with null values on the right of infix exprssions YOU MR!")
    };
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
        _ => NULL,
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
        _ => NULL,
    }
}

fn eval_if_exp(ie: IfExpression) -> Option<Object> {
    let cond = eval(Node::Expression(*ie.condition)).unwrap();
    if is_truthy(cond) {
        return eval(Node::Statement(Statement::Block(*ie.consequence)));
    } else if ie.alternative.is_some() {
        return eval(Node::Statement(Statement::Block(*ie.alternative.unwrap())));
    } else {
        Some(NULL)
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
        eval(Node::Program(program)).unwrap()
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
            test_boolean_object(&evaluated, expected);
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
                Some(expected) => test_integer_object(&evaluated, expected),
                None => test_null_object(&evaluated),
            };
        }
    }

    fn test_null_object(obj: &Object) -> bool {
        if *obj != NULL {
            eprintln!("object is not NULL. got={:?} ({:?})", obj, obj);
            false
        } else {
            true
        }
    }
}
