# Monkey Interpreter - WebAssembly Edition

A web-based Monkey programming language interpreter built with Rust and WebAssembly.

## 🚀 Live Demo

Visit the live demo at: [Your GitHub Pages URL]

## 🛠️ Local Development

### Prerequisites

- Rust (latest stable)
- wasm-pack
- A modern web browser

### Building Locally

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/rust-xp-monkey-interpreter.git
   cd rust-xp-monkey-interpreter
   ```

2. Install wasm-pack (if not already installed):
   ```bash
   cargo install wasm-pack
   ```
   > **Note**: Using `cargo install` is preferred over the shell script installer as it's more reliable and integrates better with the Rust ecosystem.

3. Build the WebAssembly module:
   ```bash
   ./build.sh
   ```

4. Open `website/index.html` in your web browser

### Manual Build Steps

If you prefer to build manually:

```bash
# Build the WebAssembly module
wasm-pack build --target web --out-dir pkg --dev

# Copy files for local testing
cp pkg/*.js website/
cp pkg/*.wasm website/
```

## 🎯 Features

- **Interactive Code Editor**: Write and edit Monkey code in a syntax-friendly environment
- **Real-time Execution**: Run your code instantly with WebAssembly
- **Example Programs**: Pre-loaded examples including Hello World, loops, and Conway's Game of Life
- **Error Handling**: Clear error messages for syntax and runtime errors
- **Performance**: Fast execution powered by WebAssembly
- **Responsive Design**: Works on desktop and mobile devices

## 📝 Monkey Language Features

The Monkey interpreter supports:

- **Data Types**: Integers, booleans, strings, arrays, and hashes
- **Variables**: Immutable bindings with `let`
- **Functions**: First-class functions with closures
- **Control Flow**: `if/else` conditionals and `while` loops
- **Built-ins**: `puts`, `len`, `first`, `last`, `rest`, `push`
- **Expressions**: Arithmetic, boolean, and comparison operations

## 🎮 Example Programs

Try these examples in the web interface:

1. **Hello World**: Simple output demonstration
2. **While Loop**: Performance test with a million iterations
3. **Game of Life**: Conway's Game of Life implementation

## 🔧 Technical Details

- **Frontend**: Vanilla JavaScript, HTML5, CSS3
- **Backend**: Rust compiled to WebAssembly
- **Build Tool**: wasm-pack
- **Deployment**: GitHub Pages with GitHub Actions

## 📁 Project Structure

```
├── src/
│   ├── wasm.rs          # WebAssembly bindings
│   ├── lib.rs           # Library entry point
│   └── ...              # Monkey interpreter core
├── website/
│   ├── index.html       # Main web interface
│   ├── styles.css       # Styling
│   ├── main.js          # JavaScript application logic
│   ├── test.html        # Test page
│   ├── monkey_rs.js     # Generated WebAssembly bindings
│   └── monkey_rs_bg.wasm # Generated WebAssembly binary
├── build.sh             # Build script
└── .github/workflows/   # GitHub Actions for deployment
```

## 🚀 Deployment

The project is automatically deployed to GitHub Pages when you push to the main branch. The GitHub Actions workflow:

1. Builds the Rust code to WebAssembly
2. Generates the necessary JavaScript bindings
3. Deploys the static files to GitHub Pages

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test locally with `./build.sh`
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- Based on the Monkey language from "Writing an Interpreter in Go" by Thorsten Ball
- Built with Rust and WebAssembly for high performance
- Inspired by the educational nature of interpreter design
