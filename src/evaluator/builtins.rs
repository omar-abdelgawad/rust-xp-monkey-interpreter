//use lazy_static::lazy_static;

use crate::object::builtins::get_builtin_by_name;
use crate::object::{Array, BuiltinObj, Object, ObjectTrait, ObjectType, NULL};

use super::new_error;

//static BUILTINS: HashMap<String, String> = HashMap::new();
//lazy_static! {
//    static ref BUILTINS: HashMap<String, Object> = {
//        let mut m = HashMap::new();
//        m.insert("len", TRUE);
//        m
//    };
//}
//static FOO: [i32; 5] = [1, 2, 3, 4, 5];

//from_iter(
//    vec![(
//        "len".to_string(),
//        Object::Builtin(BuiltinObj::new(|args: Vec<&Object>| todo!())),
//    )]
//    .iter(),
//);

// TODO: look for a way to have a global mapping
// 1.static doesn't work because you can't use non-const functions at init
// and having a specific initialization after starting program seems
// like too much for a simple mapping. But wait I think I should look it up again on how to store a
// static slice as I already have a static variable in object/builtins.rs
// 2.lazy_static is giving me errors due to Rc and RefCell from Environment
// 3. maybe use static mut and see if it is not initialized in this function and if not then
//    initialize it.
pub fn builtins(arg: &str) -> Option<BuiltinObj> {
    match arg {
        "len" => get_builtin_by_name("len"),
        "first" => get_builtin_by_name("first"),
        "last" => get_builtin_by_name("last"),
        "rest" => get_builtin_by_name("rest"),
        // TODO: the tests for push are not written yet.
        "push" => get_builtin_by_name("push"),
        "puts" => get_builtin_by_name("puts"),
        _ => None,
    }
}
