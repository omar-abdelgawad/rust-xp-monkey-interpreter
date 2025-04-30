use crate::{ast, lexer, token};
use ast::Program;

struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
}
impl Parser {
    pub fn new(l: lexer::Lexer) -> Self {
        //let p
        let mut parser = Parser {
            l,
            cur_token: token::Token::default(),
            peek_token: token::Token::default(),
        };

        // Read two tokens so cur_token and peek_token are set
        parser.next_token();
        parser.next_token();

        parser
    }
    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.l.next_token());
    }
    pub fn parse_program(self) -> Option<Program> {
        todo!()
    }
}
#[cfg(test)]
mod test {
    use crate::ast::LetStatement;
    use crate::ast::Statement;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let l = lexer::Lexer::new(input);
        let p = Parser::new(l);

        let program = p.parse_program().expect("Parse program returned None");
        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }
        let tests = vec!["x", "y", "foobar"];
        for (i, expected_ident) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            assert!(test_let_statement(stmt.as_ref(), expected_ident));
        }
    }

    fn test_let_statement(stmt: &dyn Statement, name: &str) -> bool {
        if stmt.token_literal() != "let" {
            eprintln!(
                "stmt.token_literal() not 'let'. got='{}'",
                stmt.token_literal()
            );
            return false;
        }

        // Downcast the trait object to LetStatement
        let let_stmt = stmt.as_any().downcast_ref::<LetStatement>();

        if let Some(let_stmt) = let_stmt {
            if let_stmt.name_value() != name {
                eprintln!(
                    "let_stmt.name.value not '{}'. got='{}'",
                    name,
                    let_stmt.name_value()
                );
                return false;
            }

            if let_stmt.name_token_literal() != name {
                eprintln!(
                    "let_stmt.name.token_literal() not '{}'. got='{}'",
                    name,
                    let_stmt.name_token_literal()
                );
                return false;
            }

            true
        } else {
            eprintln!("stmt is not a LetStatement. got={:?}", stmt);
            false
        }
    }
}
