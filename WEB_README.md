# Monkey Interpreter - WebAssembly Edition

A web-based Monkey programming language interpreter built with Rust and WebAssembly.

## 🚀 Live Demo

Visit the live demo at: https://omar-abdelgawad.github.io/rust-xp-monkey-interpreter/

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

3. Build the WebAssembly module:
   ```bash
   ./build.sh
   ```

4. Open `website/index.html` in your web browser


## 🧪 Local Testing

The HTTP server is already running on port 8000. You can test your interpreter at:

# Deployment Guide for Monkey WebAssembly Interpreter

## 🌐 Deploy to GitHub Pages

### Step 1: Enable GitHub Pages

1. Go to your repository on GitHub
2. Click on **Settings** tab
3. Scroll down to **Pages** section
4. Under **Source**, select **GitHub Actions**

### Step 2: Push Your Code

```bash
git add .
git commit -m "Add WebAssembly Monkey interpreter with GitHub Pages deployment"
git push origin main
```

### Step 3: Monitor Deployment

1. Go to the **Actions** tab in your GitHub repository
2. Watch the "Deploy to GitHub Pages" workflow run
3. Once complete, your site will be available at:
   `https://your-username.github.io/rust-xp-monkey-interpreter`

- **Main Interface**: http://localhost:8000/
- **Test Page**: http://localhost:8000/test.html

## 🎯 Features Available

### Code Editor
- Syntax-friendly text area
- Keyboard shortcut: `Ctrl+Enter` to run code
- Clear button to reset editor

### Example Programs
- **Hello World**: Simple output demonstration
- **While Loop**: Performance test with 1 million iterations
- **Game of Life**: Conway's Game of Life implementation

### Output Display
- Real-time execution results
- Error handling with clear messages
- Execution time display

## 🔧 Customization

### Adding New Examples

1. Add your example code to the `get_example_code` function in `src/wasm.rs`
2. Update the `get_available_examples` function
3. Rebuild with `./build.sh`

### Styling Changes

Edit `styles.css` to customize the appearance. The design is fully responsive and includes:
- Modern gradient background
- Clean card-based layout
- Syntax highlighting ready
- Mobile-friendly design

### Performance Optimization

For production deployment, use the release build:

```bash
wasm-pack build --target web --out-dir pkg --release
```