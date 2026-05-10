use std::cell::RefCell;
use wasm_bindgen::prelude::*;

use crate::ast::Node;
use crate::compiler::Compiler;
use crate::object::ObjectTrait;
use crate::parser::Parser;
use crate::vm::VM;

// Thread-local storage for output callback
thread_local! {
    static OUTPUT_CALLBACK: RefCell<Option<js_sys::Function>> = RefCell::new(None);
}

// Function to set the output callback
#[wasm_bindgen]
pub fn set_output_callback(callback: js_sys::Function) {
    OUTPUT_CALLBACK.with(|cb| {
        *cb.borrow_mut() = Some(callback);
    });
}

// Function to stream output in real-time
pub fn stream_output(text: &str) {
    OUTPUT_CALLBACK.with(|cb| {
        if let Some(ref callback) = *cb.borrow() {
            let text_js = JsValue::from_str(text);
            let _ = callback.call1(&JsValue::NULL, &text_js);
        }
    });
}


/// Bytecode VM exposed to WASM.
/// Supports incremental execution via `compile()` + `step()` loop.
#[wasm_bindgen]
pub struct MonkeyVM {
    vm: Option<VM>,
    bytecode_str: String,
}

#[wasm_bindgen]
impl MonkeyVM {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            vm: None,
            bytecode_str: String::new(),
        }
    }

    /// Parse and compile the code, creating the VM. Returns true on success.
    /// On failure, streams error messages via the output callback and returns false.
    #[wasm_bindgen]
    pub fn compile(&mut self, code: &str) -> bool {
        let mut parser = Parser::new(code.to_string());
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            let mut error_msg = String::from("Parser errors:\n");
            for error in parser.errors() {
                error_msg.push_str(&format!("  {}\n", error));
            }
            stream_output(&error_msg);
            self.vm = None;
            return false;
        }

        let mut compiler = Compiler::new();
        if let Err(err) = compiler.compile(Node::Program(program)) {
            stream_output(&format!("Compiler error: {}\n", err));
            self.vm = None;
            return false;
        }

        let bytecode = compiler.bytecode();
        let mut instructions_str = String::new();
        instructions_str.push_str("Main:\n");
        instructions_str.push_str(&bytecode.instructions.to_string());
        for (i, constant) in bytecode.constants.iter().enumerate() {
            if let crate::object::Object::CompiledFunction(func) = &**constant {
                instructions_str.push_str(&format!("\nFunction {}:\n", i));
                instructions_str.push_str(&func.instructions.to_string());
            }
        }
        self.bytecode_str = instructions_str;

        self.vm = Some(VM::new(bytecode));
        true
    }

    /// Get the formatted bytecode instructions.
    #[wasm_bindgen]
    pub fn get_instructions(&self) -> String {
        self.bytecode_str.clone()
    }

    /// Execute one bytecode instruction. Returns true on success, false on error.
    /// Call `is_running()` to check if there are more instructions.
    #[wasm_bindgen]
    pub fn step(&mut self) -> bool {
        if let Some(ref mut vm) = self.vm {
            if let Err(err) = vm.step() {
                stream_output(&format!("VM error: {}\n", err));
                return false;
            }
            true
        } else {
            false
        }
    }

    /// Returns true if the VM has more instructions to execute.
    #[wasm_bindgen]
    pub fn is_running(&self) -> bool {
        if let Some(ref vm) = self.vm {
            vm.is_running()
        } else {
            false
        }
    }

    /// Get the final result after execution completes.
    #[wasm_bindgen]
    pub fn result(&self) -> String {
        if let Some(ref vm) = self.vm {
            vm.last_popped_stack_elem().inspect()
        } else {
            "null".to_string()
        }
    }

    /// Convenience method: compile and run the entire program in one call (blocking).
    #[wasm_bindgen]
    pub fn evaluate(&mut self, code: &str) -> String {
        let mut parser = Parser::new(code.to_string());
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            let mut error_msg = String::from("Parser errors:\n");
            for error in parser.errors() {
                error_msg.push_str(&format!("  {}\n", error));
            }
            return error_msg;
        }
        let mut compiler = Compiler::new();
        if let Err(err) = compiler.compile(Node::Program(program)) {
            return format!("Compiler error: {}", err);
        }
        let bytecode = compiler.bytecode();
        let mut machine = VM::new(bytecode);
        if let Err(err) = machine.run() {
            return format!("VM error: {}", err);
        }

        let result = machine.last_popped_stack_elem();
        result.inspect()
    }

    /// Drop the inner VM.
    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.vm = None;
        self.bytecode_str.clear();
    }
}

/// Get example Monkey source code by name.
/// Uses `include_str!` to embed files at compile time from `monkey_examples/`.
#[wasm_bindgen]
pub fn get_example_code(example_name: &str) -> String {
    match example_name {
        "hello_world" => include_str!("../monkey_examples/hello_world.monkey").to_string(),
        "while_loop" => include_str!("../monkey_examples/while_loop.monkey").to_string(),
        "game_of_life" => include_str!("../monkey_examples/game_of_life.monkey").to_string(),
        _ => "puts(\"Unknown example\")".to_string(),
    }
}

#[wasm_bindgen]
pub fn get_available_examples() -> String {
    // Return a JSON string that JavaScript can parse
    r#"[
        ["hello_world", "Hello World"],
        ["while_loop", "While Loop Demo"],
        ["game_of_life", "Conway's Game of Life"]
    ]"#
    .to_string()
}
