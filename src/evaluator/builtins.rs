//use lazy_static::lazy_static;

use crate::object::builtins::get_builtin_by_name;
use crate::object::BuiltinObj;

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
        "len" | "first" | "last" | "rest" | "push" | "puts" => Some(get_builtin_by_name(arg)),
        _ => None,
    }
}
