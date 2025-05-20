use crate::ast::{ExpressionStatement, IntegerLiteral, Node, Program, Statement};
use crate::object::{Integer, Object};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Statement(stmt) => None,
        _ => None,
    }
}
fn eval_statements(stmts: Vec<Box<Statement>>) -> Option<Object> {
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
