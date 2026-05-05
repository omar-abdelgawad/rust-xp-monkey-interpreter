use super::{null_obj, BuiltinObj, Error, ObjRef, Object, ObjectTrait, ObjectType};
use std::rc::Rc;

fn new_error(formt: String) -> ObjRef {
    Rc::new(Object::Err(Error::new(formt)))
}
pub static BUILTINS: &[(&str, BuiltinObj)] = &[
    ("len", builtin_len()),     // 0
    ("puts", builtin_puts()),   // 1
    ("first", builtin_first()), // 2
    ("last", builtin_last()),   // 3
    ("rest", builtin_rest()),   // 4
    ("push", builtin_push()),   // 5
];

pub fn get_builtin_by_name(name: &str) -> Option<BuiltinObj> {
    for (def_name, def_builtin) in BUILTINS {
        if *def_name == name {
            // TODO: should return a shared reference in the future
            return Some(def_builtin.clone());
        }
    }
    None
}
const fn builtin_len() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[ObjRef]| {
            if args.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }
            match &*args[0] {
                Object::String(str_obj) => Object::new_int_var(str_obj.value.len() as i64),
                Object::Arr(arr_obj) => Object::new_int_var(arr_obj.elements.len() as i64),
                Object::Hash(hash_obj) => Object::new_int_var(hash_obj.pairs.len() as i64),
                def_obj => new_error(format!(
                    "argument to `len` not supported, got {}",
                    def_obj.r#type()
                )),
            }
        },
    }
}
const fn builtin_puts() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[ObjRef]| {
            for arg in args {
                let output = arg.inspect();
                #[cfg(target_arch = "wasm32")]
                {
                    crate::wasm::stream_output(&output);
                    crate::wasm::stream_output("\n");
                }
                #[cfg(not(target_arch = "wasm32"))]
                {
                    println!("{}", output);
                }
            }
            null_obj()
        },
    }
}

const fn builtin_first() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[ObjRef]| {
            if args.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }
            if args[0].r#type() != ObjectType::ARRAY_OBJ {
                return new_error(format!(
                    "argument to `first` must be ARRAY, got {}",
                    args[0].r#type()
                ));
            }
            let Object::Arr(arr) = &*args[0] else {
                panic!();
            };
            if !arr.elements.is_empty() {
                arr.elements[0].clone()
            } else {
                null_obj()
            }
        },
    }
}
const fn builtin_last() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[ObjRef]| {
            if args.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }
            if args[0].r#type() != ObjectType::ARRAY_OBJ {
                return new_error(format!(
                    "argument to `last` must be ARRAY, got {}",
                    args[0].r#type()
                ));
            }
            let Object::Arr(arr) = &*args[0] else {
                panic!();
            };
            let length = arr.elements.len();
            if length > 0 {
                arr.elements[length - 1].clone()
            } else {
                null_obj()
            }
        },
    }
}
const fn builtin_rest() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[ObjRef]| {
            if args.len() != 1 {
                return new_error(format!(
                    "wrong number of arguments. got={}, want=1",
                    args.len()
                ));
            }
            if args[0].r#type() != ObjectType::ARRAY_OBJ {
                return new_error(format!(
                    "argument to `rest` must be ARRAY, got {}",
                    args[0].r#type()
                ));
            }
            let Object::Arr(arr) = &*args[0] else {
                panic!();
            };
            let length = arr.elements.len();
            if length > 0 {
                let new_elements = arr.elements[1..].to_vec();
                Object::new_array_var(new_elements)
            } else {
                null_obj()
            }
        },
    }
}
const fn builtin_push() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[ObjRef]| {
            if args.len() != 2 {
                return new_error(format!(
                    "wrong number of arguments. got={}, want=2",
                    args.len()
                ));
            }
            if args[0].r#type() != ObjectType::ARRAY_OBJ {
                return new_error(format!(
                    "argument to `push` must be ARRAY, got {}",
                    args[0].r#type()
                ));
            }
            let Object::Arr(arr) = &*args[0] else {
                panic!("first argument has to be an array object.");
            };
            //let length = arr.elements.len();
            let mut new_elements = arr.elements.clone();
            new_elements.push(args[1].clone());

            Object::new_array_var(new_elements)
        },
    }
}
