pub mod environment;
use std::fmt::Display;

use environment::Environment;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{BlockStatement, Identifier};

//TODO: make all of the values below singletons with no copies
// which should be done with static.
pub const TRUE: Object = Object::Boolean(Boolean::new(true));
pub const FALSE: Object = Object::Boolean(Boolean::new(false));
pub const NULL: Object = Object::Null(Null::new());

type BuiltinFunction = fn(args: &[Object]) -> Object;

pub fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
    RETURN_VALUE_OBJ,
    ERROR_OBJ,
    FUNCTION_OBJ,
    String_OBJ,
    BuiltinFunction,
    ARRAY_OBJ,
}
impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ObjectType as ObjT;
        let mut to_write = match self {
            ObjT::Integer_OBJ => "INTEGER",
            ObjT::BOOLEAN_OBJ => "BOOLEAN",
            ObjT::NULL_OBJ => "NULL",
            ObjT::RETURN_VALUE_OBJ => "RETURN",
            ObjT::ERROR_OBJ => "ERROR",
            ObjT::FUNCTION_OBJ => "FUNCTION",
            ObjT::String_OBJ => "STRING",
            ObjT::BuiltinFunction => "builtin function",
            ObjT::ARRAY_OBJ => "ARRAY",
        };
        write!(f, "{}", to_write)
    }
}

pub trait ObjectTrait {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    Ret(ReturnValue),
    Err(Error),
    Func(Function),
    String(StringObj),
    Builtin(BuiltinObj),
    Arr(Array),
}
impl Object {
    pub fn new_int_var(value: i64) -> Object {
        Object::Integer(Integer::new(value))
    }
    pub fn new_ret_var(value: Object) -> Object {
        Object::Ret(ReturnValue::new(Box::new(value)))
    }
}
impl ObjectTrait for Object {
    fn r#type(&self) -> ObjectType {
        match self {
            Object::Integer(s) => s.r#type(),
            Object::Boolean(s) => s.r#type(),
            Object::Null(s) => s.r#type(),
            Object::Ret(s) => s.r#type(),
            Object::Err(s) => s.r#type(),
            Object::Func(s) => s.r#type(),
            Object::String(s) => s.r#type(),
            Object::Builtin(s) => s.r#type(),
            Object::Arr(s) => s.r#type(),
        }
    }
    fn inspect(&self) -> String {
        match self {
            Object::Integer(s) => s.inspect(),
            Object::Boolean(s) => s.inspect(),
            Object::Null(s) => s.inspect(),
            Object::Ret(s) => s.inspect(),
            Object::Err(s) => s.inspect(),
            Object::Func(s) => s.inspect(),
            Object::String(s) => s.inspect(),
            Object::Builtin(s) => s.inspect(),
            Object::Arr(s) => s.inspect(),
        }
    }
}

//impl Display for Object {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        match self {
//            Object::Integer(s) => write!(f, "{}", s),
//            _ => panic!(),
//        }
//    }
//}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
    pub value: i64,
}
impl Integer {
    pub fn new(value: i64) -> Self {
        Integer { value }
    }
}

impl ObjectTrait for Integer {
    fn r#type(&self) -> ObjectType {
        ObjectType::Integer_OBJ
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}
impl Boolean {
    pub const fn new(value: bool) -> Self {
        Boolean { value }
    }
}
impl ObjectTrait for Boolean {
    fn r#type(&self) -> ObjectType {
        ObjectType::BOOLEAN_OBJ
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Null;
impl Null {
    pub const fn new() -> Self {
        Null
    }
}
impl ObjectTrait for Null {
    fn r#type(&self) -> ObjectType {
        ObjectType::NULL_OBJ
    }
    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}
impl ReturnValue {
    pub fn new(value: Box<Object>) -> Self {
        ReturnValue { value }
    }
}

impl ObjectTrait for ReturnValue {
    fn r#type(&self) -> ObjectType {
        ObjectType::RETURN_VALUE_OBJ
    }
    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub message: String,
}
impl Error {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

impl ObjectTrait for Error {
    fn r#type(&self) -> ObjectType {
        ObjectType::ERROR_OBJ
    }
    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}
impl Function {
    pub fn new(
        parameters: Vec<Identifier>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            parameters,
            body,
            env,
        }
    }
}

impl ObjectTrait for Function {
    fn r#type(&self) -> ObjectType {
        ObjectType::FUNCTION_OBJ
    }
    fn inspect(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(",");
        format!("fn({}){{\n{}\n}}", params, self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringObj {
    pub value: String,
}
impl StringObj {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
        }
    }
}

impl ObjectTrait for StringObj {
    fn r#type(&self) -> ObjectType {
        ObjectType::String_OBJ
    }
    fn inspect(&self) -> String {
        format!("\"{}\"", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinObj {
    pub function: BuiltinFunction,
}
impl BuiltinObj {
    pub fn new(function: BuiltinFunction) -> Self {
        Self { function }
    }
}

impl ObjectTrait for BuiltinObj {
    fn r#type(&self) -> ObjectType {
        ObjectType::BuiltinFunction
    }
    fn inspect(&self) -> String {
        format!("builting function")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}
impl Array {
    pub fn new(elements: Vec<Object>) -> Self {
        Self { elements }
    }
}

impl ObjectTrait for Array {
    fn r#type(&self) -> ObjectType {
        ObjectType::ARRAY_OBJ
    }
    fn inspect(&self) -> String {
        let elems: Vec<String> = self.elements.iter().map(|p| p.inspect()).collect();
        format!("[{}]", elems.join(", "))
    }
}
