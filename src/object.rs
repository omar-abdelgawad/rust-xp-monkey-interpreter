#[derive(Debug)]
pub enum ObjectType {
    Integer_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
}

#[derive(Debug)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}
pub trait ObjectTrait {
    fn r#type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug)]
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
#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
}
impl ObjectTrait for Boolean {
    fn r#type(&self) -> ObjectType {
        ObjectType::BOOLEAN_OBJ
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}
#[derive(Debug)]
pub struct Null;
impl ObjectTrait for Null {
    fn r#type(&self) -> ObjectType {
        ObjectType::NULL_OBJ
    }
    fn inspect(&self) -> String {
        "null".to_string()
    }
}
