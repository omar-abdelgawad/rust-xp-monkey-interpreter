use clap::Parser;
use monkey_rs::repl::{execute_file, start};
use std::env;
use std::fs::File;
use std::io::{self, BufReader};

fn main() {
    let cli = monkey_rs::cli::Cli::parse();
    let username = get_username().unwrap_or("user".to_string());

    if let Some(filename) = cli.filename {
        match File::open(&filename) {
            Ok(file) => {
                let reader = BufReader::new(file);
                let stdout = io::stdout();
                execute_file(reader, stdout);
            }
            Err(e) => {
                eprintln!("Could not open file '{}': {}", filename, e);
                std::process::exit(1);
            }
        }
    } else {
        println!(
            "Hello {}! This is the Monkey programming language in rust!",
            username
        );
        println!("Feel free to type in commands");

        let reader = BufReader::new(io::stdin().lock());
        let stdout = io::stdout();
        start(reader, stdout);
    }
}

fn get_username() -> Option<String> {
    // Try the USER environment variable first (works on most Unix and Windows setups)
    env::var("USER").or_else(|_| env::var("USERNAME")).ok()
}
