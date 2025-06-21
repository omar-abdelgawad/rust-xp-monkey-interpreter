use lazy_static::lazy_static;
use std::collections::HashMap;

use crate::object::{BuiltinObj, Object, ObjectTrait, NULL};

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
// like too much for a simple mapping.
// 2.lazy_static is giving me errors due to Rc and RefCell from Environment
// 3. maybe use static mut and see if it is not initialized in this function and if not then
//    initialize it.
pub fn builtins(arg: &str) -> Option<BuiltinObj> {
    match arg {
        "len" => Some(BuiltinObj {
            function: |args: &[Object]| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                match &args[0] {
                    Object::String(str_obj) => Object::new_int_var(str_obj.value.len() as i64),
                    def_obj => new_error(format!(
                        "argument to \"len\" not supported, got {}",
                        def_obj.r#type()
                    )),
                }
            },
        }),
        _ => None,
    }
}
