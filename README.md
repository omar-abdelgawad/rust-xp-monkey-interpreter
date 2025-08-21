# Monkey-rs

This is an implementation of the monkey programming language from the book  [*Writing an Interpreter in Go* by Thorsten Ball](https://interpreterbook.com/) in rust. please don't ask why I did it in rust. I learned a lot of stuff though.

## 📖 About

Monkey is a small, educational programming language designed to teach interpreter concepts.  
It supports:

- Integers and booleans  
- Arithmetic and boolean expressions  
- Variable bindings (`let`)  
- First-class functions & closures  
- Conditionals (`if` / `else`)  
- Return statements  
- Strings and built-in functions  
- Hashes and arrays 

## 🚀 Getting Started

download the repo

```bash
git clone https://github.com/omar-abdelgawad/rust-xp-monkey-interpreter.git monkey-rs
cd monkey-rs
```

and then if you have [just](https://just.systems/) (the command runner) you can run the REPL with it

```bash
just run
```

or if you don't have it you can just use `cargo`

```bash
cargo run -q
```
finally you can try doing something cool with arrays and hashes like

```bash
>> let people = [{"name": "Omar", "age": 22}, {"name": "Ahmed", "age": 44}];
null
>> people[0]["name"];
"Omar"
>> let getName = fn(person) { person["name"]; };
null
>> getName(people[0]);
"Omar"
```

## 🧪 Running Tests

```bash
just test
```
