# Monkey-rs

This is an implementation of the monkey programming language from the books  [*Writing an Interpreter in Go* by Thorsten Ball](https://interpreterbook.com/) and [*Writing a Compiler in Go* by Thorsten Ball](https://compilerbook.com/) in rust. please don't ask why I did it in rust. I learned a ton of stuff though.


## 📖 About

Monkey is a small, educational programming language designed to teach interpreter concepts.  
It supports:

- Integers and booleans  
- Arithmetic and boolean expressions  
- Variable bindings (`let`)  
- Conditionals (`if` / `else`)  
- Strings and built-in functions  
- Hashes and arrays 
- Return statements  
- First-class functions & closures  

## Inspirations & Other Implementations

While implementing I had a lot of TODOs and FIXes that I couldn't really think about for too long because I wanted to finish the book first. now I have A LOT of unidiomatic rust and wrong types because I kind of ported from go and I also have some ideas that I want to implement such as wasm interactive vm with visible running instruction in bytecode. I want to record here some resources that I count as important for inspirations later so that I could look for how did someone else do the thing I want to do:

- https://github.com/ktanaka101/monkey.rs
- https://github.com/yesmeck/monkey-lang
- [Writing a 16-bit VM in rust](https://www.youtube.com/watch?v=oArXOAhzOdY&list=PLUkZG7_4JtUL22HycWYR_J-1xJo7rQGhr)

examples of TODOs in future:
- reduce cloning as much as possible.
- reduce cloning objects by making them Rc<Object>
- make step() function for executing a single instruction.
- remove all warnings.
- fix getters like current_frame()
- refactor the stack into standalone struct
- add more monkey code examples like map
- better benchmarking with criterion
- Write a better API for executing more than one compiled input. the current one is too ugly with new_with_state constructors.
- Refactor objects into single files and maybe implement Into<Object> trait for ergonomic use later.
- Add a halt instruction and exit builtin?
- Use enumKind or Write your own macros for repetitive blocks.
- I don't like the token field but removing it will break many tests unfortunately.
- Empty code blocks should return NULL.
- how to have global mappings without a function at runtime?
- make an assembler?
- the wasm interactive stuff ofc (prob hard maybe I can vibe code it)
- Also clean up the wasm module its currently really bad because it was mainly ai-generated
- trust me when I say there is much more stuff to do but I forgot.

## 🚀 Getting Started

Download the repo:

```bash
$ git clone https://github.com/omar-abdelgawad/rust-xp-monkey-interpreter.git monkey-rs
$ cd monkey-rs
$ just run
>> let people = [{"name": "Omar", "age": 22}, {"name": "Ahmed", "age": 44}];
null
>> people[0]["name"];
"Omar"
>> let getName = fn(person) { person["name"]; };
null
>> getName(people[0]);
"Omar"
```

## 🌐 Web Interface

This project includes a web-based Monkey interpreter powered by WebAssembly! 

- **Live Demo**: [GitHub Pages](https://your-username.github.io/rust-xp-monkey-interpreter)
- **Local Testing**: Run `./build.sh` then open `website/index.html` in your browser

See `website/README.md` for web interface details.

If you have [just](https://just.systems/) (the command runner), you can:

- Run the REPL:
  ```bash
  just run
  ```
- Run a Monkey script file:
  ```bash
  just run path/to/script.monkey
  ```

## 🧪 Running Tests

```bash
just test
```

## Implemented Grammar / Syntax

Notes:
    - Semicolons (;) are optional in monkey if you only have a single statement.
    - The return value of a block_statement is the value of the last statement in it.
    - All variables are immutable including Arrays and Hashes.
    - Functions are first class members and support closures.
    - Return statements are supported even outside of functions.

### Let statements

*Format*:
```
let <identifier> = <expression>;
```
*Example*:
```
let x = true;
let y = 15;
let z = y + 10;
let some_fn = fn(x) {true};
```
*Returns*:
Let statements always return NULL.
### If expressions

*Format*:
```
if (<expression>)
    { <block_statement> }
else
    { <block_statement> }
```
*Example*:
```
if(true){ 10 } else { 20 };
```
*Returns*:
Returns the return value of whichever block_statement statement is executed.

### Literals
Six types of literals are currently implemented.

1. Integer Literal

*Format*:
```
[-+]*[0-9]+;
```
*Example*:
```
+10;
-23;
128;
let x = (-56 + 123) * 2;
```

2. Boolean Literal

*Format*:
```
true | false;
```
*Example*:
```
true;
false;
let x = !false;
```
3. String Literal

*Format*:
```
"<anything_goes_here>";
```
*Example*:
```
"Hello World!";
"Hello" + " World!";
let x = "this is X";
```
4. Array Literal

*Format*:
```
[<expression>, <expression>, ...];
```
*Example*:
```
[];
[2,3,4];
[true, 2+4, "STRING HERE"];
let x = [1,false, "third element",["another array"]];
x[0];
x[-1];
x[2-1];
x[3][0];
```
5. Hash Literal

*Format*:
```
{ <expression>: <expression>, <expression>: expression, ...};
```
*Example*:
```
{};
{341: true};
let x = {"key": "val"+"ue", !false: "john"};
x["key"];
x[!true];
```
6. Function Literal

*Format*:
```
fn (<first_param_identifier>, <second_param_identifier>, ...) { <block_statement> };
```
*Function Call Format*:
```
<expression>(<expression>, <expression>, ...);
```
*Example*:
```
let fibonacci = fn(x) 
{
    if (x == 0) { 0 } 
    else { 
            if (x == 1) { 1 } 
            else {fibonacci(x - 1) + fibonacci(x - 2);}
         }
};

fibonacci(4);
```

### While loops
This is a rather non-standard feature in monkey but I really wanted some kind of loop.

*Format*:
```
while (<expression>)
    { <block_statement> }
```
*Example*:
```
let x = 3;
while( x > 0 ){ let x = x-1; };
```
*Returns*:
while loops always return NULL.


### 🛠️ Built-in Functions

Monkey provides several built-in functions for working with arrays, strings, and output. Below is a list of all available built-ins:

#### `len(obj)` → Integer
Returns the length of a string, array, or hash. Errors for other types.

*Examples:*
```monkey
len("hello");        // 5
len([1, 2, 3]);      // 3
len({"a": 1, "b": 2}); // 2
```

#### `first(array)` → Any | null
Returns the first element of an array, or `null` if the array is empty.

*Example:*
```monkey
first([10, 20, 30]); // 10
first([]);           // null
```

#### `last(array)` → Any | null
Returns the last element of an array, or `null` if the array is empty.

*Example:*
```monkey
last([10, 20, 30]); // 30
last([]);           // null
```

#### `rest(array)` → Array | null
Returns a new array containing all elements except the first. Returns `null` if the array is empty.

*Example:*
```monkey
rest([10, 20, 30]); // [20, 30]
rest([42]);         // []
rest([]);           // null
```

#### `push(array, elem)` → Array
Returns a new array with `elem` appended to the end. Does not mutate the original array.

*Example:*
```monkey
push([1, 2], 3); // [1, 2, 3]
```

#### `puts(arg1, arg2, ...)` → null
Prints all arguments to stdout, each on a separate line. Returns `null`.

*Example:*
```monkey
let x = 10;
puts(true, x + 20, {"key": x});
```
