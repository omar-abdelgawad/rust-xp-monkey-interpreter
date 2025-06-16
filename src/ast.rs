use crate::token::Token;
use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}
impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Expression(s) => write!(f, "{}", s),
            Node::Statement(s) => write!(f, "{}", s),
            Node::Program(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{}", s),
            Statement::Return(s) => write!(f, "{}", s),
            Statement::Expression(s) => write!(f, "{}", s),
            Statement::Block(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Boolean(Boolean),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression as Exp;
        match self {
            Exp::Identifier(s) => write!(f, "{}", s),
            Exp::Boolean(s) => write!(f, "{}", s),
            Exp::Integer(s) => write!(f, "{}", s),
            Exp::Prefix(s) => write!(f, "{}", s),
            Exp::Infix(s) => write!(f, "{}", s),
            Exp::If(s) => write!(f, "{}", s),
            Exp::Function(s) => write!(f, "{}", s),
            Exp::Call(s) => write!(f, "{}", s),
        }
    }
}
impl Node {
    pub fn token_literal(&self) -> String {
        match self {
            Node::Statement(stmt) => stmt.token_literal(),
            Node::Expression(expr) => expr.token_literal(),
            Node::Program(expr) => expr.token_literal(),
        }
    }
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(s) => s.token_literal(),
            Statement::Return(s) => s.token_literal(),
            Statement::Expression(s) => s.token_literal(),
            Statement::Block(s) => s.token_literal(),
        }
    }
}

impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(e) => e.token_literal(),
            Expression::Boolean(e) => e.token_literal(),
            Expression::Integer(e) => e.token_literal(),
            Expression::Prefix(e) => e.token_literal(),
            Expression::Infix(e) => e.token_literal(),
            Expression::If(e) => e.token_literal(),
            Expression::Function(e) => e.token_literal(),
            Expression::Call(e) => e.token_literal(),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}
impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Program { statements }
    }
    pub fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    token: Token, // the token.let token
    name: Identifier,
    pub value: Box<Expression>,
}
impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Box<Expression>) -> Self {
        LetStatement { token, name, value }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    // used in tests
    pub fn name_value(&self) -> String {
        self.name.value.clone()
    }
    // used in tests
    pub fn name_token_literal(&self) -> String {
        self.name.token_literal()
    }
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // e.g., "let var_name = val;"
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name,
            self.value
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone)]
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
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    token: Token, // the 'return' token
    pub return_value: Box<Expression>,
}
impl ReturnStatement {
    pub fn new(token: Token, return_value: Box<Expression>) -> Self {
        ReturnStatement {
            token,
            return_value,
        }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token_literal(), self.return_value)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    token: Token, // the first token of the Expression
    pub expression: Box<Expression>,
}
impl ExpressionStatement {
    pub fn new(token: Token, expression: Box<Expression>) -> Self {
        ExpressionStatement { token, expression }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        IntegerLiteral { token, value }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub token: Token, // the prefix token
    pub operator: String,
    pub right: Box<Expression>,
}
impl PrefixExpression {
    pub fn new(token: Token, operator: impl Into<String>, right: Box<Expression>) -> Self {
        PrefixExpression {
            token,
            operator: operator.into(),
            right,
        }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    token: Token, // the operator token
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}
impl InfixExpression {
    pub fn new(
        token: Token,
        left: Box<Expression>,
        operator: impl Into<String>,
        right: Box<Expression>,
    ) -> Self {
        InfixExpression {
            token,
            left,
            operator: operator.into(),
            right,
        }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    token: Token, // the 'if' token
    pub condition: Box<Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}
impl IfExpression {
    pub fn new(
        token: Token,
        condition: Box<Expression>,
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
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ")?;
        write!(f, "{} {}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, "else {}", alt)?;
        }
        Ok(())
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    token: Token, // the { token
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        BlockStatement { token, statements }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    token: Token, // the 'fn' token
    pub parameters: Vec<Identifier>,
    // how is this an option again?
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
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
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

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    token: Token, // the '(' token
    pub function: Box<Expression>,
    pub arguments: Vec<Box<Expression>>,
}
impl CallExpression {
    pub fn new(token: Token, function: Box<Expression>, arguments: Vec<Box<Expression>>) -> Self {
        Self {
            token,
            function,
            arguments,
        }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args: Vec<String> = self.arguments.iter().map(|p| p.to_string()).collect();
        write!(f, "{}({})", self.function, args.join(", "))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::TokenType;
    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token::new(TokenType::LET, "let"),
                name: Identifier::new(Token::new(TokenType::IDENT, "myVar"), "myVar"),
                value: Box::new(Expression::Identifier(Identifier::new(
                    Token::new(TokenType::IDENT, "anotherVar"),
                    "anotherVar",
                ))),
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
