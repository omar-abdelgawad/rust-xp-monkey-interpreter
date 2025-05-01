use crate::token::Token;
use std::any::Any;
use std::fmt::Debug;

pub trait Node: Debug {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    /// unnecessary marker function (check go marker interfaces).
    fn statement_node(&self);
    /// for testing only maybe remove it from trait later
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    /// unnecessary marker function (check go marker interfaces).
    fn expression_node(&self);
}
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}
impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}
#[derive(Debug)]
pub struct LetStatement {
    token: Token, // the token.let token
    name: Identifier,
    value: Box<dyn Expression>,
}
impl LetStatement {
    // used in tests
    pub fn name_value(&self) -> String {
        self.name.value.clone()
    }
    // used in tests
    pub fn name_token_literal(&self) -> String {
        self.name.token_literal()
    }
    pub fn new(token: Token, name: Identifier, value: Box<dyn Expression>) -> LetStatement {
        LetStatement { token, name, value }
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
#[derive(Debug, Clone)]
pub struct Identifier {
    token: Token, // the token.IDENT token
    value: String,
}
impl Identifier {
    pub fn new(token: Token, value: String) -> Identifier {
        Identifier { token, value }
    }
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}
#[derive(Debug)]
pub struct ReturnStatement {
    token: Token, // the 'return' token
    return_value: Box<dyn Expression>,
}
impl ReturnStatement {
    pub fn new(token: Token, return_value: Box<dyn Expression>) -> ReturnStatement {
        ReturnStatement {
            token,
            return_value,
        }
    }
}
impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Statement for ReturnStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
