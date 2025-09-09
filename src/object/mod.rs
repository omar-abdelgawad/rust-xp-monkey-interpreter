pub mod environment;
use fnv::FnvHasher;
use std::hash::Hasher;
use std::{collections::HashMap, fmt::Display};

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

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ObjectType {
    Integer_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
    RETURN_VALUE_OBJ,
    ERROR_OBJ,
    FUNCTION_OBJ,
    String_OBJ,
    BuiltinFunction,
    ARRAY_OBJ, // arrays are immutable in monkey
    Hash_OBJ,
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
            ObjT::Hash_OBJ => "HASH",
        };
        write!(f, "{}", to_write)
    }
}

pub trait ObjectTrait {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub trait Hashable {
    fn hash_key(&self) -> HashKey;
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct HashKey {
    pub obj_type: ObjectType,
    pub value: u64,
}
impl HashKey {
    pub fn new(obj_type: ObjectType, value: u64) -> Self {
        Self { obj_type, value }
    }
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
    Arr(Array), // arrays are immutable in monkey
    Hash(HashObj),
}
impl Object {
    pub fn new_int_var(value: i64) -> Object {
        Object::Integer(Integer::new(value))
    }
    pub fn new_str_var(value: &str) -> Object {
        Object::String(StringObj::new(value))
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
            Object::Hash(s) => s.r#type(),
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
            Object::Hash(s) => s.inspect(),
        }
    }
}
impl Hashable for Object {
    fn hash_key(&self) -> HashKey {
        match self {
            Object::Boolean(b) => b.hash_key(),
            Object::Integer(i) => i.hash_key(),
            Object::String(s) => s.hash_key(),
            _ => panic!("not a hashable object"),
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

impl Hashable for Integer {
    fn hash_key(&self) -> HashKey {
        HashKey::new(ObjectType::Integer_OBJ, self.value as u64)
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

impl Hashable for Boolean {
    fn hash_key(&self) -> HashKey {
        let val = match self.value {
            true => 1,
            false => 0,
        };
        HashKey::new(ObjectType::BOOLEAN_OBJ, val)
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

impl Hashable for StringObj {
    fn hash_key(&self) -> HashKey {
        let mut hasher = FnvHasher::default();
        hasher.write(self.value.as_bytes()); // self.value is String
        let hash = hasher.finish();

        HashKey::new(ObjectType::String_OBJ, hash)
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

#[derive(Debug, PartialEq, Clone)]
pub struct HashPair {
    pub key: Object,
    pub val: Object,
}
impl HashPair {
    pub fn new(key: Object, val: Object) -> Self {
        Self { key, val }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct HashObj {
    pub pairs: HashMap<HashKey, HashPair>,
}
impl HashObj {
    pub fn new(pairs: HashMap<HashKey, HashPair>) -> Self {
        Self { pairs }
    }

    // FIX: this should return a reference but for now we just clone.
    pub fn get(&self, index: &Object) -> Object {
        let pair = self.pairs.get(&index.hash_key());
        match pair {
            None => NULL,
            Some(p) => p.val.clone(), // should be a reference
        }
    }
}

impl ObjectTrait for HashObj {
    fn r#type(&self) -> ObjectType {
        ObjectType::Hash_OBJ
    }
    fn inspect(&self) -> String {
        let mut pairs_str = Vec::new();

        for pair in self.pairs.values() {
            pairs_str.push(format!("{}: {}", pair.key.inspect(), pair.val.inspect()));
        }

        format!("{{{}}}", pairs_str.join(", "))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_string_hash_key() {
        let hello_1 = Object::new_str_var("Hello World");
        let hello_2 = Object::new_str_var("Hello World");
        let diff1 = Object::new_str_var("My name is johnny");
        let diff2 = Object::new_str_var("My name is johnny");

        if hello_1.hash_key() != hello_2.hash_key() {
            panic!("strings with the same content have different hash keys");
        }

        if diff1.hash_key() != diff2.hash_key() {
            panic!("strings with the same content have different hash keys");
        }

        if hello_1.hash_key() == diff1.hash_key() {
            panic!("strings with different content have same hash keys");
        }
    }

    #[test]
    fn test_int_hash_key() {
        let hello_1 = Object::new_int_var(2);
        let hello_2 = Object::new_int_var(2);
        let diff1 = Object::new_int_var(5);
        let diff2 = Object::new_int_var(5);

        if hello_1.hash_key() != hello_2.hash_key() {
            panic!("integers with the same content have different hash keys");
        }

        if diff1.hash_key() != diff2.hash_key() {
            panic!("integers with the same content have different hash keys");
        }

        if hello_1.hash_key() == diff1.hash_key() {
            panic!("integers with different content have same hash keys");
        }
    }

    #[test]
    fn test_bool_hash_key() {
        let hello_1 = Object::Boolean(Boolean::new(true));
        let hello_2 = Object::Boolean(Boolean::new(true));
        let diff1 = Object::Boolean(Boolean::new(false));
        let diff2 = Object::Boolean(Boolean::new(false));

        if hello_1.hash_key() != hello_2.hash_key() {
            panic!("bools with the same content have different hash keys");
        }

        if diff1.hash_key() != diff2.hash_key() {
            panic!("bools with the same content have different hash keys");
        }

        if hello_1.hash_key() == diff1.hash_key() {
            panic!("bools with different content have same hash keys");
        }
    }
}
