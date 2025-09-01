#![allow(warnings)]
pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod token;

// Evaluation mods
pub mod evaluator;
pub mod object;

// cli
pub mod cli;

// WebAssembly bindings
#[cfg(target_arch = "wasm32")]
mod wasm;

use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
pub use wasm::*;
