# Monkey Interpreter - Web Interface

This directory contains the web interface for the Monkey programming language interpreter.

## Files

- `index.html` - Main web interface with code editor and output display
- `styles.css` - Modern, responsive styling
- `main.js` - JavaScript application logic and WebAssembly integration
- `test.html` - Simple test page for verifying WebAssembly functionality
- `monkey_rs.js` - Generated WebAssembly JavaScript bindings
- `monkey_rs_bg.wasm` - Compiled WebAssembly binary

## Local Development

To test the web interface locally:

1. Build the WebAssembly module from the project root:
   ```bash
   ./build.sh
   ```

2. Start a local HTTP server:
   ```bash
   python3 -m http.server 8000
   ```

3. Open your browser to:
   - Main interface: http://localhost:8000/website/
   - Test page: http://localhost:8000/website/test.html

## Deployment

The website is automatically deployed to GitHub Pages when you push to the main branch. The GitHub Actions workflow builds the WebAssembly module and deploys the contents of this directory.

## Features

- Interactive Monkey code editor
- Real-time code execution via WebAssembly
- Example programs (Hello World, While Loop, Game of Life)
- Error handling and output display
- Responsive design for desktop and mobile
