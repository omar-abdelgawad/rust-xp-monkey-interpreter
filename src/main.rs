use monkey_rs::repl::start;
use std::env;
use std::io::{self, BufReader};

fn main() {
    let username = get_username().unwrap_or("user".to_string());

    println!(
        "Hello {}! This is the Monkey programming language in rust!",
        username
    );
    println!("Feel free to type in commands");

    let reader = BufReader::new(io::stdin().lock());
    let stdout = io::stdout();

    start(reader, stdout);
}

fn get_username() -> Option<String> {
    // Try the USER environment variable first (works on most Unix and Windows setups)
    env::var("USER").or_else(|_| env::var("USERNAME")).ok()
}
