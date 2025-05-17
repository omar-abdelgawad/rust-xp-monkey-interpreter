use crate::token::Token;
use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;

// in the book it also has a String method to print it
pub trait Node: Debug + Display {
    fn token_literal(&self) -> String;
    /// for testing only maybe remove it from trait later
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    /// unnecessary marker function (check go marker interfaces).
    fn statement_node(&self);
}

pub trait Expression: Node {
    /// unnecessary marker function (check go marker interfaces).
    fn expression_node(&self);
}
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}
impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}
impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
#[derive(Debug)]
pub struct LetStatement {
    token: Token, // the token.let token
    name: Identifier,
    pub value: Box<dyn Expression>,
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
    pub fn new(token: Token, name: Identifier, value: Box<dyn Expression>) -> Self {
        LetStatement { token, name, value }
    }
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.token_literal())?; // e.g., "let "
        write!(f, "{}", self.name)?; // variable name
        write!(f, " = ")?;
        write!(f, "{}", self.value)?;
        write!(f, ";") // ending semicolon
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}
}
#[derive(Debug, Clone)]
pub struct Identifier {
    token: Token, // the token.IDENT token
    pub value: String,
}
impl Identifier {
    pub fn new(token: Token, value: impl Into<String>) -> Self {
        Identifier {
            token,
            value: value.into(),
        }
    }
}
// this one could probably be replaced by implementing Display
// for token only
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}
#[derive(Debug)]
pub struct Boolean {
    token: Token,
    pub value: bool,
}
impl Boolean {
    pub fn new(token: Token, value: impl Into<bool>) -> Self {
        Boolean {
            token,
            value: value.into(),
        }
    }
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for Boolean {
    fn expression_node(&self) {}
}
#[derive(Debug)]
pub struct ReturnStatement {
    token: Token, // the 'return' token
    pub return_value: Box<dyn Expression>,
}
impl ReturnStatement {
    pub fn new(token: Token, return_value: Box<dyn Expression>) -> Self {
        ReturnStatement {
            token,
            return_value,
        }
    }
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.token_literal())?;
        write!(f, "{};", self.return_value)
    }
}
impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}
#[derive(Debug)]
pub struct ExpressionStatement {
    token: Token, // the first token of the Expression
    pub expression: Box<dyn Expression>,
}
impl ExpressionStatement {
    pub fn new(token: Token, expression: Box<dyn Expression>) -> Self {
        ExpressionStatement { token, expression }
    }
}
impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}
#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        IntegerLiteral { token, value }
    }
}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}
impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}
#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token, // the prefix token
    pub operator: String,
    pub right: Box<dyn Expression>,
}
impl PrefixExpression {
    pub fn new(token: Token, operator: impl Into<String>, right: Box<dyn Expression>) -> Self {
        PrefixExpression {
            token,
            operator: operator.into(),
            right,
        }
    }
}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}
impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}
#[derive(Debug)]
pub struct InfixExpression {
    token: Token, // the operator token
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}
impl InfixExpression {
    pub fn new(
        token: Token,
        left: Box<dyn Expression>,
        operator: impl Into<String>,
        right: Box<dyn Expression>,
    ) -> Self {
        InfixExpression {
            token,
            left,
            operator: operator.into(),
            right,
        }
    }
}
impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for InfixExpression {
    fn expression_node(&self) {}
}
#[derive(Debug)]
pub struct IfExpression {
    token: Token, // the 'if' token
    pub condition: Box<dyn Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}
impl IfExpression {
    pub fn new(
        token: Token,
        condition: Box<dyn Expression>,
        consequence: Box<BlockStatement>,
        alternative: Option<Box<BlockStatement>>,
    ) -> Self {
        Self {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}
impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if")?;
        write!(f, "{}", self.condition)?;
        write!(f, " ")?;
        write!(f, "{}", self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}
impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for IfExpression {
    fn expression_node(&self) {}
}
#[derive(Debug)]
pub struct BlockStatement {
    token: Token, // the { token
    pub statements: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Box<dyn Statement>>) -> Self {
        BlockStatement { token, statements }
    }
}
impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }
        Ok(())
        //write!(f, "{} ", self.token_literal())?; // e.g., "let "
        //write!(f, "{}", self.name)?; // variable name
        //write!(f, " = ")?;
        //write!(f, "{}", self.value)?;
        //write!(f, ";") // ending semicolon
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Statement for BlockStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct FunctionLiteral {
    token: Token, // the 'fn' token
    pub parameters: Vec<Identifier>,
    pub body: Option<Box<BlockStatement>>,
}
impl FunctionLiteral {
    pub fn new(
        token: Token,
        parameters: Vec<Identifier>,
        body: Option<Box<BlockStatement>>,
    ) -> Self {
        Self {
            token,
            parameters,
            body,
        }
    }
}
impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "{}({})", self.token.literal, params.join(", "))?;
        if let Some(body) = &self.body {
            write!(f, " {}", body)
        } else {
            write!(f, " {{}}")
        }
    }
}
impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct CallExpression {
    token: Token, // the '(' token
    pub funciton: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}
impl CallExpression {
    pub fn new(
        token: Token,
        funciton: Box<dyn Expression>,
        arguments: Vec<Box<dyn Expression>>,
    ) -> Self {
        Self {
            token,
            funciton,
            arguments,
        }
    }
}
impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args: Vec<String> = self.arguments.iter().map(|p| p.to_string()).collect();
        write!(f, "{}({})", self.funciton, args.join(", "))
    }
}
impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl Expression for CallExpression {
    fn expression_node(&self) {}
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::TokenType;
    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::new(TokenType::LET, "let"),
                name: Identifier::new(Token::new(TokenType::IDENT, "myVar"), "myVar"),
                value: Box::new(Identifier::new(
                    Token::new(TokenType::IDENT, "anotherVar"),
                    "anotherVar",
                )),
            })],
        };
        let prog_str = format!("{}", program);
        assert_eq!(
            prog_str, "let myVar = anotherVar;",
            "prog_str wrong. got {}",
            prog_str
        );
    }
}
