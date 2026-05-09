use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::{Hash, Hasher};

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

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Boolean(Boolean),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Str(StringLiteral),
    Arr(ArrayLiteral),
    Ind(IndexExpression),
    Hash(HashLiteral),
    While(WhileExpression), // always returns NULL
}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // intentionally left things that can't evaluate to str, int oro bool
        match self {
            Expression::Integer(i) => i.hash(state),
            Expression::Boolean(b) => b.hash(state),
            Expression::Str(s) => s.hash(state),
            Expression::Identifier(ident) => ident.hash(state),
            Expression::Infix(inf) => inf.hash(state),
            Expression::Prefix(pre) => pre.hash(state),
            Expression::If(if_exp) => if_exp.hash(state),
            Expression::Call(call_exp) => call_exp.hash(state),
            Expression::Ind(ind_exp) => ind_exp.hash(state),
            def => panic!(
                "rust tried to hash something not a str or int or bool (not hashing inside the language): {}",
                def
            ),
        }
    }
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
            Exp::Str(s) => write!(f, "{}", s),
            Exp::Arr(s) => write!(f, "{}", s),
            Exp::Ind(s) => write!(f, "{}", s),
            Exp::Hash(s) => write!(f, "{}", s),
            Exp::While(s) => write!(f, "{}", s),
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
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Box<Expression>,
}
impl LetStatement {
    pub fn new(name: Identifier, value: Box<Expression>) -> Self {
        LetStatement { name, value }
    }
    pub fn name_value(&self) -> String {
        self.name.value.clone()
    }
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // e.g., "let var_name = val;"
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub value: String,
}
impl Identifier {
    pub fn new(value: impl Into<String>) -> Self {
        Identifier {
            value: value.into(),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Boolean {
    pub value: bool, // TODO: integer literals should store the str representation but does that
                     // apply here as well?
}
impl Boolean {
    pub fn new(value: impl Into<bool>) -> Self {
        Boolean {
            value: value.into(),
        }
    }
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct ReturnStatement {
    pub return_value: Box<Expression>,
}
impl ReturnStatement {
    pub fn new(return_value: Box<Expression>) -> Self {
        ReturnStatement { return_value }
    }
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}
impl ExpressionStatement {
    pub fn new(expression: Box<Expression>) -> Self {
        ExpressionStatement { expression }
    }
}
impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct IntegerLiteral {
    pub value: i64, // should probably store the string itself and evaluate it later
}
impl IntegerLiteral {
    pub fn new(value: i64) -> Self {
        IntegerLiteral { value }
    }
}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<Expression>,
}
impl PrefixExpression {
    pub fn new(operator: impl Into<String>, right: Box<Expression>) -> Self {
        PrefixExpression {
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
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}
impl InfixExpression {
    pub fn new(left: Box<Expression>, operator: impl Into<String>, right: Box<Expression>) -> Self {
        InfixExpression {
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
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}
impl IfExpression {
    pub fn new(
        condition: Box<Expression>,
        consequence: Box<BlockStatement>,
        alternative: Option<Box<BlockStatement>>,
    ) -> Self {
        Self {
            condition,
            consequence,
            alternative,
        }
    }
}
impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {{{}}}", self.condition, self.consequence)?;
        if let Some(alt) = &self.alternative {
            write!(f, " else {{{}}}", alt)?;
        }
        Ok(())
    }
}
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(statements: Vec<Statement>) -> Self {
        BlockStatement { statements }
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

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: Box<BlockStatement>,
    pub name: String,
}
impl FunctionLiteral {
    pub fn new(parameters: Vec<Identifier>, body: Box<BlockStatement>) -> Self {
        Self {
            parameters,
            body,
            name: "".to_string(),
        }
    }
}
impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "fn")?;
        if !self.name.is_empty() {
            write!(f, "<{}>", self.name)?;
        }
        write!(f, "({})", params.join(", "))?;
        write!(f, " {}", self.body)
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}
impl CallExpression {
    pub fn new(function: Box<Expression>, arguments: Vec<Expression>) -> Self {
        Self {
            function,
            arguments,
        }
    }
}
impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args: Vec<String> = self.arguments.iter().map(|p| p.to_string()).collect();
        write!(f, "{}({})", self.function, args.join(", "))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub value: String,
}
impl StringLiteral {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
        }
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}
impl ArrayLiteral {
    pub fn new(elements: Vec<Expression>) -> Self {
        Self { elements }
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elems: Vec<String> = self.elements.iter().map(|p| p.to_string()).collect();
        write!(f, "[{}]", elems.join(", "))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}
impl IndexExpression {
    pub fn new(left: Box<Expression>, index: Box<Expression>) -> Self {
        Self { left, index }
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashLiteral {
    pub pairs: HashMap<Expression, Expression>,
}

impl HashLiteral {
    pub fn new(pairs: HashMap<Expression, Expression>) -> Self {
        Self { pairs }
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pairs: Vec<String> = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}:{}", k, v))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct WhileExpression {
    pub condition: Box<Expression>,
    pub loop_body: Box<BlockStatement>,
}
impl WhileExpression {
    pub fn new(condition: Box<Expression>, loop_body: Box<BlockStatement>) -> Self {
        Self {
            condition,
            loop_body,
        }
    }
}
impl Display for WhileExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ")?;
        write!(f, "{} {}", self.condition, self.loop_body)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                name: Identifier::new("myVar"),
                value: Box::new(Expression::Identifier(Identifier::new("anotherVar"))),
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
