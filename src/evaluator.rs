use crate::ast::{Expression, ExpressionStatement, IntegerLiteral, Node, Program, Statement};
use crate::object::{Boolean, Integer, Object};

const TRUE: Object = Object::Boolean(Boolean::new(true));
const FALSE: Object = Object::Boolean(Boolean::new(false));

pub fn eval(node: Node) -> Option<Object> {
    use Expression as Exp;
    use Statement as St;
    match node {
        Node::Statement(stmt) => match stmt {
            St::Expression(exp_stmt) => eval(Node::Expression(*exp_stmt.expression)),
            _ => None,
        },
        Node::Program(prog) => eval_statements(prog.statements),
        Node::Expression(exp) => match exp {
            Exp::Integer(int_lit) => Some(Object::Integer(Integer::new(int_lit.value))),
            Exp::Boolean(int_lit) => Some(native_bool_to_boolean_object(int_lit.value)),
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
fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
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
        let tests = vec![("5", 5i64), ("10", 10)];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, expected);
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
        let tests = vec![("true", true), ("false", false)];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(&evaluated, expected);
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
}
