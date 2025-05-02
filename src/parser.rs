use crate::{
    ast::{
        self, Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement,
        ReturnStatement, Statement,
    },
    lexer,
    token::{self, TokenType},
};
use ast::Program;
use std::collections::HashMap;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[repr(u8)]
pub enum Precedence {
    LOWEST = 1,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}
type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> Option<Box<dyn Expression>>;
struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
}
impl Parser {
    pub fn new(l: lexer::Lexer) -> Self {
        //let p
        let mut parser = Parser {
            l,
            cur_token: token::Token::default(),
            peek_token: token::Token::default(),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        // Read two tokens so cur_token and peek_token are set
        parser.next_token();
        parser.next_token();

        parser.register_prefix(TokenType::IDENT, Parser::parse_identifier);
        parser.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        parser
    }
    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier::new(
            self.cur_token.clone(),
            self.cur_token.literal.clone(),
        )))
    }
    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        let value = self.cur_token.literal.parse::<i64>();
        if let Ok(value) = value {
            Some(Box::new(IntegerLiteral::new(cur_token_tmp, value)))
        } else {
            None
        }
    }
    fn errors(&self) -> &[String] {
        &self.errors // Returns a reference to the vector as a slice
    }
    fn peek_error(&mut self, t: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.ttype
        );
        self.errors.push(msg);
    }
    fn next_token(&mut self) {
        // equivalent to:
        // p.curToken = p.peekToken
        // p.peekToken = p.l.NextToken()
        self.cur_token = std::mem::replace(&mut self.peek_token, self.l.next_token());
    }
    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.ttype {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let cur_token_tmp = self.cur_token.clone();
        let stmt =
            ExpressionStatement::new(cur_token_tmp, self.parse_expression(Precedence::LOWEST)?);
        // check for optional SEMICOLON
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(stmt))
    }
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.ttype)?;
        let left_exp = prefix(self);
        left_exp
    }
    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let cur_token_tmp = self.cur_token.clone();
        self.next_token();
        // fake expression used here
        let ident = Identifier::new(cur_token_tmp.clone(), cur_token_tmp.literal.clone());
        let stmt = ReturnStatement::new(cur_token_tmp, Box::new(ident));
        // TODO: We're skipping the expression until SEMICOLON
        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(stmt))
    }
    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let cur_token_tmp = self.cur_token.clone();
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }
        let ident = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());
        let stmt = LetStatement::new(cur_token_tmp, ident.clone(), Box::new(ident.clone()));
        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }
        // TODO: we're skipping the expressions until SEMICOLON
        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(stmt))
    }
    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.ttype == t
    }
    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.ttype == t
    }
    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = ast::Program { statements: vec![] };
        while !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        program
    }
    fn register_prefix(&mut self, token_type: token::TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: token::TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }
}
#[cfg(test)]
mod test {
    use crate::ast::ExpressionStatement;
    use crate::ast::LetStatement;
    use crate::ast::Node;
    use crate::ast::ReturnStatement;
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
        let mut p = Parser::new(l);

        let program = p.parse_program(); //.expect("Parse program returned None");
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements doesnt contain 3 statements. got={}",
            program.statements.len()
        );
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
    fn check_parser_errors(p: Parser) {
        let errors = p.errors();
        if errors.len() == 0 {
            return;
        }
        eprintln!("parser had {} errors", errors.len());
        for (i, msg) in errors.iter().enumerate() {
            eprintln!("{}. parser error: {}", i, msg);
        }
        panic!();
    }
    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program(); //.expect("Parse program returned None");
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements doesnt contain 3 statements. got={}",
            program.statements.len()
        );
        for (i, stmt) in program.statements.iter().enumerate() {
            let return_stmt = stmt.as_ref().as_any().downcast_ref::<ReturnStatement>();
            if let Some(return_stmt) = return_stmt {
                assert_eq!(
                    return_stmt.token_literal(),
                    "return",
                    "return_stmt.token_literal() not 'return', got {}",
                    return_stmt.token_literal()
                );
            } else {
                panic!("stmt not ast::ReturnStatement. got={:?}", stmt);
            }
        }
    }
    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements. got={:?}",
            program.statements.len()
        );
        let express_stmt = program.statements[0]
            .as_ref()
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect(
                format!(
                    "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                    program.statements[0]
                )
                .as_str(),
            );
        let ident = express_stmt
            .expression
            .as_ref()
            .as_any()
            .downcast_ref::<Identifier>()
            .expect(format!("exp not ast::Identifier. got={:?}", express_stmt.expression).as_str());
        assert_eq!(
            ident.value, "foobar",
            "ident.value not {}. got={:?}",
            "foobar", ident.value
        );
    }
    #[test]
    fn test_integer_literal_exp() {
        let input = "5;";
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements. got={:?}",
            program.statements.len()
        );
        let express_stmt = program.statements[0]
            .as_ref()
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .expect(
                format!(
                    "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                    program.statements[0]
                )
                .as_str(),
            );
        let literal = express_stmt
            .expression
            .as_ref()
            .as_any()
            .downcast_ref::<IntegerLiteral>()
            .expect(
                format!(
                    "exp not ast::IntegerLiteral. got={:?}",
                    express_stmt.expression
                )
                .as_str(),
            );
        assert_eq!(
            literal.value, 5,
            "literal.value not {}. got={:?}",
            5, literal.value
        );
        assert_eq!(
            literal.token_literal(),
            "5",
            "literal.token_literal() not {}. got={}",
            "5",
            literal.token_literal()
        );
    }
}
