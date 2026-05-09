//#![allow(warnings)]
pub mod ast;
pub mod parser;
pub mod repl;

// Evaluation mods
pub mod evaluator;
pub mod object;

// cli
pub mod cli;

// WebAssembly bindings
#[cfg(target_arch = "wasm32")]
mod wasm;

// --------mods after reading compiler book--------
pub mod code;
pub mod compiler;
pub mod vm;
