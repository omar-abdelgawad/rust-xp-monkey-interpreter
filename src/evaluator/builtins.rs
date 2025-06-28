//use lazy_static::lazy_static;
use std::collections::HashMap;

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
                    Object::Arr(arr_obj) => Object::new_int_var(arr_obj.elements.len() as i64),
                    def_obj => new_error(format!(
                        "argument to \"len\" not supported, got {}",
                        def_obj.r#type()
                    )),
                }
            },
        }),
        // TODO: the tests for the following builtins are not written yet.
        "first" => Some(BuiltinObj {
            function: |args: &[Object]| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                if args[0].r#type() != ObjectType::ARRAY_OBJ {
                    return new_error(format!(
                        "argument to `first` must be ARRAY, got={}",
                        args[0].r#type()
                    ));
                }
                let Object::Arr(arr) = &args[0] else {
                    panic!();
                };
                if arr.elements.len() > 0 {
                    return arr.elements[0].clone();
                }
                NULL
            },
        }),
        "last" => Some(BuiltinObj {
            function: |args: &[Object]| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                if args[0].r#type() != ObjectType::ARRAY_OBJ {
                    return new_error(format!(
                        "argument to `first` must be ARRAY, got={}",
                        args[0].r#type()
                    ));
                }
                let Object::Arr(arr) = &args[0] else {
                    panic!();
                };
                let length = arr.elements.len();
                if length > 0 {
                    return arr.elements[length - 1].clone();
                }
                NULL
            },
        }),
        "rest" => Some(BuiltinObj {
            function: |args: &[Object]| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }
                if args[0].r#type() != ObjectType::ARRAY_OBJ {
                    return new_error(format!(
                        "argument to `first` must be ARRAY, got={}",
                        args[0].r#type()
                    ));
                }
                let Object::Arr(arr) = &args[0] else {
                    panic!();
                };
                let length = arr.elements.len();
                if length > 0 {
                    let new_elements = arr.elements[1..].to_vec();
                    return Object::Arr(Array::new(new_elements));
                }
                NULL
            },
        }),
        "push" => Some(BuiltinObj {
            function: |args: &[Object]| {
                if args.len() != 2 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=2",
                        args.len()
                    ));
                }
                if args[0].r#type() != ObjectType::ARRAY_OBJ {
                    return new_error(format!(
                        "argument to `first` must be ARRAY, got={}",
                        args[0].r#type()
                    ));
                }
                let Object::Arr(arr) = &args[0] else {
                    panic!();
                };
                let length = arr.elements.len();
                let mut new_elements = arr.elements.clone();
                new_elements.push(args[1].clone());
                return Object::Arr(Array::new(new_elements));
            },
        }),
        _ => None,
    }
}
