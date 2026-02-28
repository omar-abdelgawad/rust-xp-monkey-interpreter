use super::{Array, BuiltinObj, Error, Object, ObjectTrait, ObjectType, NULL};

fn new_error(formt: String) -> Object {
    Object::Err(Error::new(formt))
}
pub static BUILTINS: &[(&'static str, BuiltinObj)] = &[
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
        function: |args: &[Object]| {
            #[cfg(target_arch = "wasm32")]
            {
                let args_cloned = args.to_vec();
                fn step(mut args: Vec<Object>) {
                    if args.is_empty() {
                        return;
                    }

                    let arg = args.remove(0);
                    let output = arg.inspect();

                    // Streaming output
                    crate::wasm::stream_output(&output);
                    if !output.ends_with('\n') {
                        crate::wasm::stream_output("\n");
                    }

                    // Schedule the next step to let the DOM paint
                    if !args.is_empty() {
                        yield_to_paint(move || step(args));
                    }
                }

                step(args_cloned);
                return NULL;
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                for arg in args {
                    let output = arg.inspect();
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        // For native compilation, use standard println
                        println!("{}", output);
                    }
                }

                NULL
            }
        },
    }
}

const fn builtin_first() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[Object]| {
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
            let Object::Arr(arr) = &args[0] else {
                panic!();
            };
            if !arr.elements.is_empty() {
                arr.elements[0].clone()
            } else {
                NULL
            }
        },
    }
}
const fn builtin_last() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[Object]| {
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
            let Object::Arr(arr) = &args[0] else {
                panic!();
            };
            let length = arr.elements.len();
            if length > 0 {
                arr.elements[length - 1].clone()
            } else {
                NULL
            }
        },
    }
}
const fn builtin_rest() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[Object]| {
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
            let Object::Arr(arr) = &args[0] else {
                panic!();
            };
            let length = arr.elements.len();
            if length > 0 {
                let new_elements = arr.elements[1..].to_vec();
                Object::Arr(Array::new(new_elements))
            } else {
                NULL
            }
        },
    }
}
const fn builtin_push() -> BuiltinObj {
    BuiltinObj {
        function: |args: &[Object]| {
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
            let Object::Arr(arr) = &args[0] else {
                panic!("first argument has to be an array object.");
            };
            //let length = arr.elements.len();
            let mut new_elements = arr.elements.clone();
            new_elements.push(args[1].clone());

            Object::Arr(Array::new(new_elements))
        },
    }
}
#[cfg(target_arch = "wasm32")]
pub fn yield_to_paint<F>(f: F)
where
    F: 'static + FnOnce(),
{
    use wasm_bindgen::closure::Closure;
    use wasm_bindgen::prelude::*;
    use wasm_bindgen::JsCast;
    use web_sys::window;

    let closure = Closure::once_into_js(f);
    window()
        .unwrap()
        .request_animation_frame(closure.as_ref().unchecked_ref())
        .unwrap();
    // closure consumed by once_into_js, no need to forget
}
