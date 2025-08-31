use clap::Parser;

/// Monkey interpreter in Rust
#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    /// Monkey script file to execute
    pub filename: Option<String>,
}
