use crate::token::Token;
use std::any::Any;
use std::fmt::Debug;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node + Debug {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node + Debug {
    fn expression_node(&self);
}
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
    token: Token,
    name: Identifier,
    value: Box<dyn Expression>,
}
impl LetStatement {
    pub fn name_value(&self) -> String {
        self.name.value.clone()
    }
    pub fn name_token_literal(&self) -> String {
        self.name.token.literal.clone()
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
#[derive(Debug)]
struct Identifier {
    token: Token,
    value: String,
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}
