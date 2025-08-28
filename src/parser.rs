use crate::{
    ast::{
        self, ArrayLiteral, BlockStatement, Boolean, CallExpression, Expression,
        ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression,
        IndexExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
        ReturnStatement, Statement, StringLiteral, WhileExpression,
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
    INDEX,       // myArray[ind]
}
type PrefixParseFn = fn(&mut Parser) -> Option<Box<Expression>>;
type InfixParseFn = fn(&mut Parser, Box<Expression>) -> Option<Box<Expression>>;
pub struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
}
impl Parser {
    pub fn new(l: lexer::Lexer) -> Self {
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
        parser.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::PLUS, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::TRUE, Parser::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Parser::parse_boolean);
        parser.register_prefix(TokenType::FUNCTION, Parser::parse_function_literal);
        parser.register_prefix(TokenType::LPAREN, Parser::parse_grouped_expression);
        parser.register_prefix(TokenType::IF, Parser::parse_if_expression);
        parser.register_prefix(TokenType::STRING, Parser::parse_string_literal);
        parser.register_prefix(TokenType::LBRACKET, Parser::parse_array_literal);
        parser.register_prefix(TokenType::LBRACE, Parser::parse_hash_literal);
        parser.register_prefix(TokenType::WHILE, Parser::parse_while_expression);
        parser.register_infix(TokenType::PLUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Parser::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Parser::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Parser::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Parser::parse_infix_expression);
        parser.register_infix(TokenType::NOT_EQ, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::GT, Parser::parse_infix_expression);
        parser.register_infix(TokenType::LPAREN, Parser::parse_call_expression);
        parser.register_infix(TokenType::LBRACKET, Parser::parse_index_expression);

        parser
    }

    fn parse_while_expression(&mut self) -> Option<Box<Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }
        self.next_token();
        let cond_tmp = self.parse_expression(Precedence::LOWEST).unwrap();
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }
        let loop_body = self.parse_block_statement().unwrap();
        let exp = WhileExpression::new(cur_token_tmp, cond_tmp, loop_body);
        Some(Box::new(Expression::While(exp)))
    }

    fn parse_hash_literal(&mut self) -> Option<Box<Expression>> {
        let tmp_tok = self.cur_token.clone();
        let mut tmp_pairs = HashMap::new();
        while !self.peek_token_is(TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST).unwrap();
            if !self.expect_peek(TokenType::COLON) {
                return None;
            }
            self.next_token();
            let value = self.parse_expression(Precedence::LOWEST).unwrap();
            tmp_pairs.insert(*key, *value);
            if !self.peek_token_is(TokenType::RBRACE) && !self.expect_peek(TokenType::COMMA) {
                return None;
            }
        }
        if !self.expect_peek(TokenType::RBRACE) {
            return None;
        }
        Some(Box::new(Expression::Hash(HashLiteral::new(
            tmp_tok, tmp_pairs,
        ))))
    }
    fn parse_index_expression(&mut self, left: Box<Expression>) -> Option<Box<Expression>> {
        let tmp_tok = self.cur_token.clone();
        self.next_token();
        let exp = IndexExpression::new(tmp_tok, left, self.parse_expression(Precedence::LOWEST)?);
        if !self.expect_peek(TokenType::RBRACKET) {
            return None;
        }
        Some(Box::new(Expression::Ind(exp)))
    }

    fn parse_array_literal(&mut self) -> Option<Box<Expression>> {
        let tmp_tok = self.cur_token.clone();
        let tmp_elems = self.parse_expression_list(TokenType::RBRACKET)?;
        Some(Box::new(Expression::Arr(ArrayLiteral::new(
            tmp_tok, tmp_elems,
        ))))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
        let mut list: Vec<Expression> = Vec::new();

        if self.peek_token_is(end.clone()) {
            self.next_token();
            return Some(list);
        }
        self.next_token();
        list.push(*self.parse_expression(Precedence::LOWEST)?);
        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            list.push(*self.parse_expression(Precedence::LOWEST)?);
        }
        if !self.expect_peek(end.clone()) {
            return None;
        }
        Some(list)
    }

    fn parse_string_literal(&mut self) -> Option<Box<Expression>> {
        let str_lit = StringLiteral::new(self.cur_token.clone(), self.cur_token.literal.clone());
        let exp = Expression::Str(str_lit);
        Some(Box::new(exp))
    }
    fn parse_call_expression(&mut self, function: Box<Expression>) -> Option<Box<Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        let exp = CallExpression::new(cur_token_tmp, function, self.parse_call_arguments()?);
        Some(Box::new(Expression::Call(exp)))
    }
    // remove this function and replace callsite with parse_expression_list but you need to change
    // type to Vec<Expression> by removing the box
    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args: Vec<Expression> = vec![];
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(args);
        }
        self.next_token();
        args.push(*self.parse_expression(Precedence::LOWEST)?);
        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            args.push(*self.parse_expression(Precedence::LOWEST)?);
        }
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        Some(args)
    }
    fn parse_function_literal(&mut self) -> Option<Box<Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }
        let param_tmp = self.parse_function_parameters()?;
        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }
        let lit = FunctionLiteral::new(cur_token_tmp, param_tmp, self.parse_block_statement());
        Some(Box::new(Expression::Function(lit)))
    }
    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(identifiers);
        }
        self.next_token();
        let ident = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());
        identifiers.push(ident);
        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());
            identifiers.push(ident);
        }
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        Some(identifiers)
    }
    fn parse_if_expression(&mut self) -> Option<Box<Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }
        self.next_token();
        let cond_tmp = self.parse_expression(Precedence::LOWEST).unwrap();
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }
        let consq_tmp = self.parse_block_statement().unwrap();
        let mut alt_tmp = None;
        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            if !self.expect_peek(TokenType::LBRACE) {
                return None;
            }
            alt_tmp = self.parse_block_statement();
        }
        let exp = IfExpression::new(cur_token_tmp, cond_tmp, consq_tmp, alt_tmp);
        Some(Box::new(Expression::If(exp)))
    }
    fn parse_block_statement(&mut self) -> Option<Box<BlockStatement>> {
        let cur_token_tmp = self.cur_token.clone();
        let mut stmts_tmp: Vec<Statement> = vec![];
        self.next_token();
        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                stmts_tmp.push(*stmt);
            }
            self.next_token();
        }
        let block = BlockStatement::new(cur_token_tmp, stmts_tmp);
        Some(Box::new(block))
    }
    fn parse_grouped_expression(&mut self) -> Option<Box<Expression>> {
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST);
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        exp
    }
    fn parse_boolean(&mut self) -> Option<Box<Expression>> {
        let exp = Boolean::new(self.cur_token.clone(), self.cur_token_is(TokenType::TRUE));
        Some(Box::new(Expression::Boolean(exp)))
    }
    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Option<Box<Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        let operator_tmp = self.cur_token.literal.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let exp = InfixExpression::new(
            cur_token_tmp,
            left,
            operator_tmp,
            self.parse_expression(precedence)?,
        );
        Some(Box::new(Expression::Infix(exp)))
    }
    fn parse_prefix_expression(&mut self) -> Option<Box<Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        let operator_tmp = self.cur_token.literal.clone();
        self.next_token();
        let exp = PrefixExpression::new(
            cur_token_tmp,
            operator_tmp,
            self.parse_expression(Precedence::PREFIX)?,
        );
        Some(Box::new(Expression::Prefix(exp)))
    }
    fn parse_identifier(&mut self) -> Option<Box<Expression>> {
        Some(Box::new(Expression::Identifier(Identifier::new(
            self.cur_token.clone(),
            self.cur_token.literal.clone(),
        ))))
    }
    fn parse_integer_literal(&mut self) -> Option<Box<Expression>> {
        let cur_token_tmp = self.cur_token.clone();
        let value = self.cur_token.literal.parse::<i64>();
        if let Ok(value) = value {
            Some(Box::new(Expression::Integer(IntegerLiteral::new(
                cur_token_tmp,
                value,
            ))))
        } else {
            None
        }
    }
    pub fn errors(&self) -> &[String] {
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
    fn parse_statement(&mut self) -> Option<Box<Statement>> {
        match self.cur_token.ttype {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    fn parse_expression_statement(&mut self) -> Option<Box<Statement>> {
        let cur_token_tmp = self.cur_token.clone();
        let stmt =
            ExpressionStatement::new(cur_token_tmp, self.parse_expression(Precedence::LOWEST)?);
        // check for optional SEMICOLON
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(Statement::Expression(stmt)))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.ttype);
        if prefix.is_none() {
            self.no_prefix_parse_fn_error(self.cur_token.ttype.clone());
            return None;
        }
        let mut left_exp = prefix.unwrap()(self)?;
        // note that SEMICOLON check is not necessary
        // since default precedence if not found is LOWEST
        // but this is more explicit and clear
        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            // Get infix function *first*, clone or copy the reference to drop the immutable borrow
            let infix_fn = match self.infix_parse_fns.get(&self.peek_token.ttype).copied() {
                Some(func) => func,
                None => return Some(left_exp),
            };

            self.next_token(); // Now safe to mutably borrow
            left_exp = infix_fn(self, left_exp)?;
        }

        Some(left_exp)
    }
    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        self.errors
            .push(format!("no prefix parse function for {:?} found", t));
    }
    fn parse_return_statement(&mut self) -> Option<Box<Statement>> {
        let cur_token_tmp = self.cur_token.clone();
        self.next_token();
        let ret_val_tmp = self.parse_expression(Precedence::LOWEST)?;
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        let stmt = ReturnStatement::new(cur_token_tmp, ret_val_tmp);
        Some(Box::new(Statement::Return(stmt)))
    }
    fn parse_let_statement(&mut self) -> Option<Box<Statement>> {
        let cur_token_tmp = self.cur_token.clone();
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }
        let name_tmp = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());
        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }
        self.next_token();
        let stmt = LetStatement::new(
            cur_token_tmp,
            name_tmp,
            self.parse_expression(Precedence::LOWEST)?,
        );
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Box::new(Statement::Let(stmt)))
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
        let mut program = Program::new(vec![]);
        while !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(*stmt);
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
    fn peek_precedence(&self) -> Precedence {
        Parser::precedences(&self.peek_token.ttype)
    }
    fn cur_precedence(&self) -> Precedence {
        Parser::precedences(&self.cur_token.ttype)
    }
    pub fn precedences(token: &TokenType) -> Precedence {
        match token {
            TokenType::EQ => Precedence::EQUALS,
            TokenType::NOT_EQ => Precedence::EQUALS,
            TokenType::LT => Precedence::LESSGREATER,
            TokenType::GT => Precedence::LESSGREATER,
            TokenType::PLUS => Precedence::SUM,
            TokenType::MINUS => Precedence::SUM,
            TokenType::SLASH => Precedence::PRODUCT,
            TokenType::ASTERISK => Precedence::PRODUCT,
            TokenType::LPAREN => Precedence::CALL,
            TokenType::LBRACKET => Precedence::INDEX,
            _ => Precedence::LOWEST,
        }
    }
}
#[cfg(test)]
mod test {
    use std::collections::hash_map::Values;

    use crate::ast::Boolean;
    use crate::ast::CallExpression;
    use crate::ast::ExpressionStatement;
    use crate::ast::FunctionLiteral;
    use crate::ast::LetStatement;
    use crate::ast::Node;
    use crate::ast::ReturnStatement;
    use crate::ast::Statement;
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn test_let_statements() {
        let tests: Vec<(&str, &str, &dyn Any)> = vec![
            ("let x = 5;", "x", &5),
            ("let y = true;", "y", &true),
            ("let foobar = y;", "foobar", &"y"),
        ];
        for (input, expected_ident, expected_val) in tests {
            let l = lexer::Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements doesnt contain 1 statements. got={}",
                program.statements.len()
            );
            let stmt = &program.statements[0];
            if !test_let_statement(stmt, expected_ident) {
                panic!()
            }

            match stmt {
                Statement::Let(let_stmt) => {
                    assert!(test_literal_expression(&let_stmt.value, expected_val));
                }
                _ => panic!(),
            }
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) -> bool {
        if stmt.token_literal() != "let" {
            eprintln!(
                "stmt.token_literal() not 'let'. got='{}'",
                stmt.token_literal()
            );
            return false;
        }

        let Statement::Let(let_stmt) = stmt else {
            eprintln!("stmt is not a LetStatement. got={:?}", stmt);
            return false;
        };

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
    }
    fn check_parser_errors(p: Parser) {
        let errors = p.errors();
        if errors.is_empty() {
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
        let tests: Vec<(&str, &dyn Any)> = vec![
            ("return 5;", &5),
            ("return true;", &true),
            ("return foobar;", &"foobar"),
        ];
        for (input, expected_val) in tests {
            let l = lexer::Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements doesnt contain 1 statements. got={}",
                program.statements.len()
            );
            let stmt = &program.statements[0];
            let Statement::Return(ret_stmt) = stmt else {
                panic!("stmt not ReturnStatement. got={:?}", stmt);
            };
            assert_eq!(
                ret_stmt.token_literal(),
                "return",
                "ret_stmt.token_literal() not 'return', got={:?}",
                ret_stmt.token_literal()
            );
            if !test_literal_expression(&ret_stmt.return_value, expected_val) {
                panic!()
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
        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Identifier(ident) = &(*express_stmt.expression) else {
            panic!("exp not ast::Identifier. got={:?}", express_stmt.expression);
        };
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
        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Integer(literal) = &*express_stmt.expression else {
            panic!(
                "exp not ast::IntegerLiteral. got={:?}",
                express_stmt.expression
            );
        };
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

    #[test]
    fn test_boolean_exp() {
        let input = "true;";
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
        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Boolean(literal) = &*express_stmt.expression else {
            panic!("exp not ast::Boolean. got={:?}", express_stmt.expression);
        };
        assert_eq!(
            literal.value, true,
            "literal.value not {}. got={:?}",
            true, literal.value
        );
        assert_eq!(
            literal.token_literal(),
            "true",
            "literal.token_literal() not {}. got={}",
            "true",
            literal.token_literal()
        );
    }
    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests: Vec<(&str, &str, &dyn Any)> = vec![
            ("!5;", "!", &5i64),
            ("-15;", "-", &15i64),
            ("!true;", "!", &true),
            ("!false;", "!", &false),
            ("!false", "!", &false), // TODO: why does this also work?
            ("+15;", "+", &15i64),
        ];
        for (input, operator, integer_value) in prefix_tests {
            let l = lexer::Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain {}, statements. got={}",
                1,
                program.statements.len()
            );

            let Statement::Expression(express_stmt) = &program.statements[0] else {
                panic!(
                    "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                    program.statements[0]
                );
            };
            let Expression::Prefix(exp) = &*express_stmt.expression else {
                panic!(
                    "exp not ast::IntegerLiteral. got={:?}",
                    express_stmt.expression
                );
            };
            assert_eq!(
                exp.operator, operator,
                "exp.operator is not {}. got={}",
                operator, exp.operator
            );
            assert!(test_literal_expression(&exp.right, integer_value));
        }
    }
    fn test_integer_literal(il: &Expression, value: i64) -> bool {
        let Expression::Integer(integ) = il else {
            panic!("il not ast::IntegerLiteral. got={:?}", il);
        };
        if integ.value != value {
            eprintln!("integ.value not {}. got={}", value, integ.value);
            return false;
        }
        if integ.token_literal() != format!("{}", value) {
            eprintln!(
                "integ.token_literal() not {}. got={}",
                value,
                integ.token_literal()
            );
            return false;
        }
        true
    }
    #[test]
    fn test_parsing_infix_expressions_bool_version() {
        let infix_tests: Vec<(&str, &dyn Any, &str, &dyn Any)> = vec![
            ("5 + 5;", &5i64, "+", &5i64),
            ("5 - 5;", &5i64, "-", &5i64),
            ("5 * 5;", &5i64, "*", &5i64),
            ("5 / 5;", &5i64, "/", &5i64),
            ("5 > 5;", &5i64, ">", &5i64),
            ("5 < 5;", &5i64, "<", &5i64),
            ("5 == 5;", &5i64, "==", &5i64),
            ("5 != 5;", &5i64, "!=", &5i64),
            ("true == true", &true, "==", &true),
            ("true != false", &true, "!=", &false),
            ("false == false", &false, "==", &false),
        ];
        for (input, left_val, op, right_val) in infix_tests {
            let l = lexer::Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);
            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain {}, statements. got={}",
                1,
                program.statements.len()
            );
            let Statement::Expression(express_stmt) = &program.statements[0] else {
                panic!(
                    "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                    program.statements[0]
                );
            };
            let Expression::Infix(exp) = &*express_stmt.expression else {
                panic!(
                    "exp not ast::InfixExpression. got={:?}",
                    express_stmt.expression
                );
            };
            assert_eq!(
                exp.operator, op,
                "exp.operator is not {}. got={}",
                op, exp.operator
            );
            assert!(test_literal_expression(&exp.right, right_val));
        }
    }
    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];
        for (input, expected) in tests {
            let l = lexer::Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);
            let actual = program.to_string();
            assert_eq!(actual, expected, "expected {}, got={}", expected, actual);
        }
    }
    fn test_identifier(exp: &Expression, value: &str) -> bool {
        let Expression::Identifier(ident) = exp else {
            eprintln!("exp not ast::Identifier. got={}", exp);
            return false;
        };
        if ident.value != value {
            eprintln!("ident.value not {}. got={}", value, ident.value);
            return false;
        }
        if ident.token_literal() != value {
            eprintln!(
                "ident.token_literal() not {}. got={}",
                value,
                ident.token_literal()
            );
            return false;
        }
        true
    }
    use std::any::Any;
    fn test_literal_expression(exp: &Expression, expected: &dyn Any) -> bool {
        if let Some(&int_val) = expected.downcast_ref::<i64>() {
            return test_integer_literal(exp, int_val);
        }
        if let Some(&int_val) = expected.downcast_ref::<i32>() {
            return test_integer_literal(exp, int_val as i64);
        }
        if let Some(str_val) = expected.downcast_ref::<&str>() {
            return test_identifier(exp, str_val);
        }
        if let Some(&bool_val) = expected.downcast_ref::<bool>() {
            return test_boolean_literal(exp, bool_val);
        }
        eprintln!("type of exp not handled. got={:?}", exp);
        false
    }
    fn test_boolean_literal(exp: &Expression, value: bool) -> bool {
        let Expression::Boolean(bool_exp) = exp else {
            eprintln!("exp not ast::Boolean. got={}", exp);
            return false;
        };
        if bool_exp.token_literal() != format!("{}", value) {
            eprintln!(
                "integ.token_literal() not {}. got={}",
                value,
                bool_exp.token_literal()
            );
            return false;
        }
        true
    }
    fn test_infix_expression(
        exp: &Expression,
        left: &dyn Any,
        operator: &str,
        right: &dyn Any,
    ) -> bool {
        let Expression::Infix(op_exp) = exp else {
            eprintln!("exp not ast::InfixExpression. got={}", exp);
            return false;
        };
        if !test_literal_expression(&op_exp.left, left) {
            return false;
        }

        if op_exp.operator != operator {
            eprintln!(
                "exp.operator is not '{}'. got='{}'",
                operator, op_exp.operator
            );
            return false;
        }

        if !test_literal_expression(&op_exp.right, right) {
            return false;
        }
        true
    }
    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            1,
            "program.Body does not contain {}, statements. got={}",
            1,
            program.statements.len()
        );
        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::If(exp) = &*express_stmt.expression else {
            panic!(
                "exp not ast::InfixExpression. got={:?}",
                express_stmt.expression
            );
        };
        if !test_infix_expression(&exp.condition, &"x", "<", &"y") {
            panic!()
        }
        assert_eq!(
            exp.consequence.statements.len(),
            1,
            "consequence is not 1 statements. got={}",
            exp.consequence.statements.len()
        );

        let Expression::If(exp) = &*express_stmt.expression else {
            panic!(
                "exp not ast::IfExpression. got={:?}",
                express_stmt.expression
            );
        };
        let Statement::Expression(consequence) = &exp.consequence.statements[0] else {
            panic!(
                "statements[0] is not ast::ExpressionStatement. got={:?}",
                exp.consequence.statements[0]
            );
        };

        if !test_identifier(&consequence.expression, "x") {
            panic!()
        }
        if let Some(alt) = &exp.alternative {
            panic!(
                "exp.alternative.statements was not None. got={:?}",
                exp.alternative
            )
        }
    }
    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            1,
            "program.Body does not contain {}, statements. got={}",
            1,
            program.statements.len()
        );

        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::If(exp) = &*express_stmt.expression else {
            panic!(
                "exp not ast::PrefixExpression. got={:?}",
                express_stmt.expression
            );
        };
        if !test_infix_expression(&exp.condition, &"x", "<", &"y") {
            panic!()
        }
        assert_eq!(
            exp.consequence.statements.len(),
            1,
            "consequence is not 1 statements. got={}",
            exp.consequence.statements.len()
        );

        let Statement::Expression(consequence) = &exp.consequence.statements[0] else {
            panic!(
                "statements[0] is not ast::ExpressionStatement. got={:?}",
                exp.consequence.statements[0]
            );
        };
        if !test_identifier(&consequence.expression, "x") {
            panic!()
        }
        let alt = exp
            .alternative
            .as_ref()
            .expect("should have alternative block with y inside.");
        assert_eq!(
            alt.statements.len(),
            1,
            "consequence is not 1 statements. got={}",
            alt.statements.len()
        );

        let Statement::Expression(alt) = &alt.statements[0] else {
            panic!(
                "statements[0] is not ast::ExpressionStatement. got={:?}",
                alt.statements[0]
            );
        };
        if !test_identifier(&alt.expression, "y") {
            panic!()
        }
    }
    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) {x + y}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            1,
            "program.Body does not contain {}, statements. got={}",
            1,
            program.statements.len()
        );
        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Function(function) = &*express_stmt.expression else {
            panic!(
                "exp not ast::FunctionLiteral. got={:?}",
                express_stmt.expression
            );
        };
        assert_eq!(
            function.parameters.len(),
            2,
            "function literal parameters wrong. want 2, got={}",
            function.parameters.len()
        );

        // idk why we don't use the ret_val of both test functions down
        let boxed: Box<Expression> =
            Box::new(Expression::Identifier(function.parameters[0].clone()));
        test_literal_expression(&boxed, &"x");
        let boxed: Box<Expression> =
            Box::new(Expression::Identifier(function.parameters[1].clone()));
        test_literal_expression(&boxed, &"y");
        if let Some(body) = &function.body {
            assert_eq!(
                body.statements.len(),
                1,
                "function.body.statements has not 1 statements. got={}",
                body.statements.len()
            );
            let Statement::Expression(body_stmt) = &body.statements[0] else {
                panic!(
                    "function body stmt is not ast::ExpressionStatement. got={:?}",
                    body.statements[0]
                );
            };
            test_infix_expression(&body_stmt.expression, &"x", "+", &"y");
        } else {
            panic!()
        }
    }
    #[test]
    fn test_function_parameter_parsing() {
        let tests: Vec<(&str, Vec<&str>)> = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {}", vec!["x", "y", "z"]),
        ];
        for (input, expected_params) in tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);
            let Statement::Expression(express_stmt) = &program.statements[0] else {
                panic!();
            };
            let Expression::Function(function) = &*express_stmt.expression else {
                panic!();
            };
            assert_eq!(
                function.parameters.len(),
                expected_params.len(),
                "length parameters wrong. want {}, got={}\n",
                expected_params.len(),
                function.parameters.len()
            );
            for (i, ident) in expected_params.iter().enumerate() {
                let boxed: Box<Expression> =
                    Box::new(Expression::Identifier(function.parameters[i].clone()));
                test_literal_expression(&boxed, ident);
            }
        }
    }
    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            1,
            "program.Body does not contain {}, statements. got={}",
            1,
            program.statements.len()
        );

        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Call(exp) = &*express_stmt.expression else {
            panic!(
                "exp not ast::FunctionLiteral. got={:?}",
                express_stmt.expression
            );
        };
        if !test_identifier(&exp.function, "add") {
            panic!()
        }
        assert_eq!(
            exp.arguments.len(),
            3,
            "wrong length of arguments. got={}",
            exp.arguments.len()
        );
        assert!(test_literal_expression(&exp.arguments[0], &1));
        assert!(test_infix_expression(&exp.arguments[1], &2, "*", &3));
        assert!(test_infix_expression(&exp.arguments[2], &4, "+", &5));
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\"";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        assert_eq!(program.statements.len(), 1);
        let Statement::Expression(exp_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Str(str_lit) = &*exp_stmt.expression else {
            panic!("exp not ast::StringLiteral. got={:?}", exp_stmt);
        };
        assert_eq!(str_lit.value, "hello world");
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        assert_eq!(program.statements.len(), 1);
        let Statement::Expression(exp_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Arr(arr_lit) = &*exp_stmt.expression else {
            panic!("exp not ast::ArrayLiteral. got={:?}", exp_stmt);
        };
        assert_eq!(
            arr_lit.elements.len(),
            3,
            "len(arr_lit.elements) not 3. got={}",
            arr_lit.elements.len()
        );
        assert!(test_integer_literal(
            &Box::new(arr_lit.elements[0].clone()),
            1
        ));
        assert!(test_infix_expression(
            &Box::new(arr_lit.elements[1].clone()),
            &2,
            "*",
            &2
        ));

        assert!(test_infix_expression(
            &Box::new(arr_lit.elements[2].clone()),
            &3,
            "+",
            &3
        ));
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        assert_eq!(program.statements.len(), 1);
        let Statement::Expression(exp_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Ind(index_exp) = &*exp_stmt.expression else {
            panic!("exp not ast::IndexExpression. got={}", exp_stmt.expression)
        };
        assert!(test_identifier(&index_exp.left, "myArray"));
        assert!(test_infix_expression(&index_exp.index, &1, "+", &1));
    }

    #[test]
    fn test_parsing_hash_literal_string_keys() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        let Statement::Expression(exp_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Hash(hash_lit) = &*exp_stmt.expression else {
            panic!("exp not ast::Hash. got={}", exp_stmt.expression)
        };
        assert_eq!(
            hash_lit.pairs.len(),
            3,
            "hash_lit.pairs has wrong length. got={}",
            hash_lit.pairs.len()
        );
        let expected: HashMap<&str, i64> =
            HashMap::from_iter(vec![("one", 1), ("two", 2), ("three", 3)]);
        for (key_exp, val_exp) in &hash_lit.pairs {
            let key = match key_exp {
                Expression::Str(s) => &s.value,
                _ => panic!("key is not ast::StringLiteral. got={:?}", key_exp),
            };

            let expected_value = expected
                .get(key.as_str())
                .unwrap_or_else(|| panic!("No value found for key {}", key));

            let val = match val_exp {
                Expression::Integer(i) => i.value,
                _ => panic!("value is not ast::IntegerLiteral. got={:?}", val_exp),
            };

            assert_eq!(
                &val, expected_value,
                "value mismatch for key {}. expected={}, got={}",
                key, expected_value, val
            );
        }
    }

    #[test]
    fn test_parsing_empty_hash_lit() {
        let input = "{}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        let Statement::Expression(exp_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Hash(hash_lit) = &*exp_stmt.expression else {
            panic!("exp not ast::Hash. got={}", exp_stmt.expression)
        };

        assert_eq!(
            hash_lit.pairs.len(),
            0,
            "hash_lit.pairs has wrong length. got={}",
            hash_lit.pairs.len()
        );
    }

    #[test]
    fn test_parsing_hash_literal_with_expression() {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        let Statement::Expression(exp_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::Hash(hash_lit) = &*exp_stmt.expression else {
            panic!("exp not ast::Hash. got={}", exp_stmt.expression)
        };

        assert_eq!(
            hash_lit.pairs.len(),
            3,
            "hash_lit.pairs has wrong length. got={}",
            hash_lit.pairs.len()
        );

        // Expected mapping from key -> test function
        let mut tests: HashMap<&str, Box<dyn Fn(&Expression)>> = HashMap::new();
        tests.insert(
            "one",
            Box::new(|e: &Expression| {
                test_infix_expression(e, &0, "+", &1);
            }),
        );
        tests.insert(
            "two",
            Box::new(|e: &Expression| {
                test_infix_expression(e, &10, "-", &8);
            }),
        );
        tests.insert(
            "three",
            Box::new(|e: &Expression| {
                test_infix_expression(e, &15, "/", &5);
            }),
        );

        for (key_exp, val_exp) in &hash_lit.pairs {
            let key = match key_exp {
                Expression::Str(s) => s.value.as_str(),
                _ => panic!("key is not ast::StringLiteral. got={:?}", key_exp),
            };

            let Some(test_fn) = tests.get(key) else {
                panic!("No test function for key {:?}", key);
            };

            test_fn(val_exp);
        }
    }

    #[test]
    fn test_while_expression() {
        let input = "while (x < y) { x }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert_eq!(
            program.statements.len(),
            1,
            "program.Body does not contain {}, statements. got={}",
            1,
            program.statements.len()
        );
        let Statement::Expression(express_stmt) = &program.statements[0] else {
            panic!(
                "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                program.statements[0]
            );
        };
        let Expression::While(exp) = &*express_stmt.expression else {
            panic!(
                "exp not ast::WhileExprssion. got={:?}",
                express_stmt.expression
            );
        };
        if !test_infix_expression(&exp.condition, &"x", "<", &"y") {
            panic!()
        }
        assert_eq!(
            exp.loop_body.statements.len(),
            1,
            "loop_body is not 1 statements. got={}",
            exp.loop_body.statements.len()
        );

        let Statement::Expression(loop_body) = &exp.loop_body.statements[0] else {
            panic!(
                "statements[0] is not ast::ExpressionStatement. got={:?}",
                exp.loop_body.statements[0]
            );
        };

        if !test_identifier(&loop_body.expression, "x") {
            panic!()
        }
    }
}
