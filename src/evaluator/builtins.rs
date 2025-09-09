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
                    Object::Hash(hash_obj) => Object::new_int_var(hash_obj.pairs.len() as i64),
                    def_obj => new_error(format!(
                        "argument to \"len\" not supported, got {}",
                        def_obj.r#type()
                    )),
                }
            },
        }),
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
                    arr.elements[0].clone()
                } else {
                    NULL
                }
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
                        "argument to `last` must be ARRAY, got={}",
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
                        "argument to `rest` must be ARRAY, got={}",
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
        }),
        // TODO: the tests for push are not written yet.
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
                        "argument to `push` must be ARRAY, got={}",
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
        "puts" => Some(BuiltinObj {
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
                    //use wasm_bindgen_futures::spawn_local;
                    //spawn_local(async move {
                    //    for arg in args_cloned {
                    //        let output = arg.inspect();
                    //        // For WebAssembly, use streaming output
                    //        crate::wasm::stream_output(&output);
                    //        // Ensure each puts call ends with a newline
                    //        if !output.ends_with('\n') {
                    //            crate::wasm::stream_output("\n");
                    //        }
                    //        yield_to_paint().await;
                    //    }
                    //});
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
                    return NULL;
                }
            },
        }),
        _ => None,
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

//#[cfg(target_arch = "wasm32")]
//async fn yield_to_paint() {
//    use futures_channel::oneshot;
//    use wasm_bindgen::closure::Closure;
//    use wasm_bindgen::prelude::*;
//    use wasm_bindgen::JsCast;
//    use web_sys::window;
//    let (tx, rx) = oneshot::channel();
//
//    let cb = Closure::once_into_js(move || {
//        let _ = tx.send(());
//    });
//
//    window()
//        .unwrap()
//        .request_animation_frame(cb.as_ref().unchecked_ref())
//        .unwrap();
//
//    // Wait until the callback fires on next paint
//    rx.await.unwrap();
//}
