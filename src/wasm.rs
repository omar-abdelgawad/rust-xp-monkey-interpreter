use wasm_bindgen::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::Node;
use crate::evaluator;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::object::ObjectTrait;
use crate::parser::Parser;

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





#[wasm_bindgen]
pub struct MonkeyInterpreter {
    env: Rc<RefCell<Environment>>,
}

#[wasm_bindgen]
impl MonkeyInterpreter {
    #[wasm_bindgen(constructor)]
    pub fn new() -> MonkeyInterpreter {
        MonkeyInterpreter {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    #[wasm_bindgen]
    pub fn evaluate(&mut self, code: &str) -> String {
        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            let mut error_msg = String::from("Parser errors:\n");
            for error in parser.errors() {
                error_msg.push_str(&format!("  {}\n", error));
            }
            return error_msg;
        }

        let result = evaluator::eval(Node::Program(program), Rc::clone(&self.env));
        result.inspect()
    }



    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.env = Rc::new(RefCell::new(Environment::new()));
    }
}



// Function to get example code
#[wasm_bindgen]
pub fn get_example_code(example_name: &str) -> String {
    match example_name {
        "hello_world" => "puts(\"Hello, World!\")".to_string(),
        "while_loop" => r#"
# This is kind of a speed test for while loop

let i = 1000000; # 1 million
while (i > 0) {
    let i = i - 1;
}"#.to_string(),
        "game_of_life" => {
            let mut code = String::new();
            code.push_str("############################################################\n");
            code.push_str("# Conway's Game of Life — Monkey implementation (finite grid)\n");
            code.push_str("# - Live cell: 1\n");
            code.push_str("# - Dead cell: 0\n");
            code.push_str("# - Out-of-bounds neighbors count as 0 (no wraparound)\n");
            code.push_str("############################################################\n\n");
            code.push_str("# --- Grid utilities ---\n");
            code.push_str("let rows = fn(g) { len(g) };\n\n");
            code.push_str("let cols = fn(g) {\n");
            code.push_str("    # assume first element exists\n");
            code.push_str("    len(first(g))\n");
            code.push_str("};\n\n");
            code.push_str("# Safe cell access: returns 0 when (r,c) is out of bounds\n");
            code.push_str("let cell = fn(g, r, c) {\n");
            code.push_str("    if (r < 0) { 0 } \n");
            code.push_str("    else {\n");
            code.push_str("        if (r >= rows(g)) { 0 } \n");
            code.push_str("        else {\n");
            code.push_str("            let row = g[r];\n");
            code.push_str("            if (c < 0) { 0 } \n");
            code.push_str("            else {\n");
            code.push_str("                if (c >= cols(g)) { 0 } else {row[c]}\n");
            code.push_str("            }\n");
            code.push_str("        }\n");
            code.push_str("    }\n");
            code.push_str("};\n\n");
            code.push_str("# Count the 8 neighbors around (r, c)\n");
            code.push_str("let neighbor_count = fn(g, r, c) {\n");
            code.push_str("    cell(g, r-1, c-1) +\n");
            code.push_str("    cell(g, r-1, c  ) +\n");
            code.push_str("    cell(g, r-1, c+1) +\n");
            code.push_str("    cell(g, r  , c-1) +\n");
            code.push_str("    cell(g, r  , c+1) +\n");
            code.push_str("    cell(g, r+1, c-1) +\n");
            code.push_str("    cell(g, r+1, c  ) +\n");
            code.push_str("    cell(g, r+1, c+1)\n");
            code.push_str("};\n\n");
            code.push_str("# Game of Life rule for a single cell\n");
            code.push_str("let next_cell = fn(g, r, c) {\n");
            code.push_str("    let alive = cell(g, r, c);\n");
            code.push_str("    let n = neighbor_count(g, r, c);\n\n");
            code.push_str("    if (alive == 1) {\n");
            code.push_str("        # Survival on 2 or 3 neighbors\n");
            code.push_str("        if (n == 2) { 1 } else {\n");
            code.push_str("        if (n == 3) { 1 } else { 0 }}\n");
            code.push_str("    } else {\n");
            code.push_str("        # Birth on exactly 3 neighbors\n");
            code.push_str("        if (n == 3) { 1 } else { 0 }\n");
            code.push_str("    }\n");
            code.push_str("};\n\n");
            code.push_str("# Build next row r as a new array\n");
            code.push_str("let evolve_row = fn(g, r) {\n");
            code.push_str("    let c = 0;\n");
            code.push_str("    let C = cols(g);\n");
            code.push_str("    let acc = [];\n");
            code.push_str("    while (c < C) {\n");
            code.push_str("        let acc = push(acc, next_cell(g, r, c));\n");
            code.push_str("        let c = c + 1;\n");
            code.push_str("    };\n");
            code.push_str("    acc\n");
            code.push_str("};\n\n");
            code.push_str("# Build next generation grid as a new array of arrays\n");
            code.push_str("let evolve = fn(g) {\n");
            code.push_str("    let r = 0;\n");
            code.push_str("    let R = rows(g);\n");
            code.push_str("    let acc = [];\n");
            code.push_str("    while (r < R) {\n");
            code.push_str("        let acc = push(acc, evolve_row(g, r));\n");
            code.push_str("        let r = r + 1;\n");
            code.push_str("    };\n");
            code.push_str("    acc\n");
            code.push_str("};\n\n");
            code.push_str("# --- Pretty printer for the grid ---\n");
            code.push_str("let row_to_string = fn(row) {\n");
            code.push_str("    let i = 0;\n");
            code.push_str("    let s = \"\";\n");
            code.push_str("    let n = len(row);\n");
            code.push_str("    while (i < n) {\n");
            code.push_str("        let ch = if (row[i] == 1) { \"#\" } else { \".\" };\n");
            code.push_str("        let s = s + ch;\n");
            code.push_str("        let i = i + 1;\n");
            code.push_str("    };\n");
            code.push_str("    s\n");
            code.push_str("};\n\n");
            code.push_str("let print_grid = fn(g) {\n");
            code.push_str("    let r = 0;\n");
            code.push_str("    let R = rows(g);\n");
            code.push_str("    while (r < R) {\n");
            code.push_str("        puts(row_to_string(g[r]));\n");
            code.push_str("        let r = r + 1;\n");
            code.push_str("    };\n");
            code.push_str("};\n\n");
            code.push_str("############################################################\n");
            code.push_str("# Demo: run a few generations of a glider on a 10x10 board\n");
            code.push_str("############################################################\n\n");
            code.push_str("# Place a small glider in the top-left\n");
            code.push_str("let seed = [\n");
            code.push_str("    [0,1,0,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,1,0,0,0,0,0,0,0],\n");
            code.push_str("    [1,1,1,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,0,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,0,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,0,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,0,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,0,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,0,0,0,0,0,0,0,0],\n");
            code.push_str("    [0,0,0,0,0,0,0,0,0,0]\n");
            code.push_str("];\n\n");
            code.push_str("# Run N generations, printing each\n");
            code.push_str("let run = fn(g, gens) {\n");
            code.push_str("    let i = 0;\n");
            code.push_str("    while (i < gens+1) {\n");
            code.push_str("        puts(\"Generation \" + i);\n");
            code.push_str("        print_grid(g);\n");
            code.push_str("        puts(\"\");  # blank line\n");
            code.push_str("        let g = evolve(g);\n");
            code.push_str("        let i = i + 1;\n");
            code.push_str("    };\n");
            code.push_str("};\n\n");
            code.push_str("# --- Execute demo (change gens to taste) ---\n");
            code.push_str("puts(\"starting\");\n");
            code.push_str("run(seed, 5);");
            code
        },
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
    ]"#.to_string()
}
