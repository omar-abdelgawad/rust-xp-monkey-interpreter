# Deployment Guide for Monkey WebAssembly Interpreter

## 🚀 Quick Start

Your Monkey interpreter is now ready for deployment to GitHub Pages! Here's what we've built:

### ✅ What's Complete

1. **WebAssembly Build System**: Your Rust Monkey interpreter compiles to WebAssembly
2. **Web Interface**: Beautiful, responsive web interface with code editor
3. **Example Integration**: All your Monkey examples are available in the web interface
4. **GitHub Actions**: Automated deployment workflow
5. **Local Testing**: HTTP server running on port 8000

### 📁 Files Created

- `website/index.html` - Main web interface
- `website/styles.css` - Modern, responsive styling
- `website/main.js` - JavaScript application logic
- `website/monkey_rs.js` - WebAssembly JavaScript bindings
- `website/monkey_rs_bg.wasm` - Compiled WebAssembly binary
- `build.sh` - Build script for local development
- `.github/workflows/deploy.yml` - GitHub Actions deployment
- `website/test.html` - Simple test page

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

## 🧪 Local Testing

The HTTP server is already running on port 8000. You can test your interpreter at:

- **Main Interface**: http://localhost:8000/website/
- **Test Page**: http://localhost:8000/website/test.html

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

## 🐛 Troubleshooting

### Build Issues

If you encounter build errors:

1. Ensure you have the latest Rust toolchain: `rustup update`
2. Install wasm-pack: `cargo install wasm-pack`
3. Clean and rebuild: `rm -rf pkg/ target/ && ./build.sh`

