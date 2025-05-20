#[derive(Debug)]
enum ObjectType {
    Integer_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
}

trait Object {
    fn r#type() -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug)]
struct Integer {
    value: i64,
}

impl Object for Integer {
    fn r#type() -> ObjectType {
        ObjectType::Integer_OBJ
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}
#[derive(Debug)]
struct Boolean {
    value: bool,
}
impl Object for Boolean {
    fn r#type() -> ObjectType {
        ObjectType::BOOLEAN_OBJ
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}
#[derive(Debug)]
struct Null;
impl Object for Null {
    fn r#type() -> ObjectType {
        ObjectType::NULL_OBJ
    }
    fn inspect(&self) -> String {
        "null".to_string()
    }
}
