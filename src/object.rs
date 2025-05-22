//TODO: make all of the values below singletons with no copies
pub const TRUE: Object = Object::Boolean(Boolean::new(true));
pub const FALSE: Object = Object::Boolean(Boolean::new(false));
pub const NULL: Object = Object::Null(Null::new());

pub fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

#[derive(Debug)]
pub enum ObjectType {
    Integer_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
}

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}
impl Object {
    pub fn new_int_var(value: i64) -> Object {
        Object::Integer(Integer::new(value))
    }
}
impl ObjectTrait for Object {
    fn r#type(&self) -> ObjectType {
        match self {
            Object::Integer(s) => s.r#type(),
            Object::Boolean(s) => s.r#type(),
            Object::Null(s) => s.r#type(),
        }
    }
    fn inspect(&self) -> String {
        match self {
            Object::Integer(s) => s.inspect(),
            Object::Boolean(s) => s.inspect(),
            Object::Null(s) => s.inspect(),
        }
    }
}
pub trait ObjectTrait {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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
