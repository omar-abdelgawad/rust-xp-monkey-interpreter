use crate::ast::{ExpressionStatement, IntegerLiteral, Node, Program, Statement};
use crate::object::{Integer, Object};

pub fn eval(node: Box<dyn Node>) -> Option<Object> {
    if let Some(int_lit) = node.as_any().downcast_ref::<IntegerLiteral>() {
        Some(Object::Integer(Integer::new(int_lit.value)))
    } else if let Some(program) = node.as_any().downcast_ref::<Program>() {
        None
        //eval_statements(program.statements)
    } else if let Some(expression_stmt) = node.as_any().downcast_ref::<ExpressionStatement>() {
        None
        //eval(
        //    expression as Box<dyn Node>, //.as_any()
        //                                 //.downcast_ref::<Box<dyn Node>>()
        //                                 //.unwrap(),
        //)
    } else if let Some(statement) = node.as_any().downcast_ref::<Program>() {
        None
    } else {
        None
    }
}
fn eval_statements(stmts: Vec<Box<dyn Statement>>) -> Option<Object> {
    let mut result = None;
    for statement in stmts {
        result = eval(statement);
    }
    result
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
        eval(Box::new(program)).unwrap()
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
}
