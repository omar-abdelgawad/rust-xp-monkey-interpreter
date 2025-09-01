#!/bin/bash

# Build script for Monkey WebAssembly interpreter

set -e

echo "🐒 Building Monkey WebAssembly Interpreter..."

# Check if wasm-pack is installed
if ! command -v wasm-pack &> /dev/null; then
    echo "❌ wasm-pack is not installed. Installing via cargo..."
    cargo install wasm-pack
fi

# Clean previous builds
echo "🧹 Cleaning previous builds..."
rm -rf pkg/
rm -rf target/

# Build the WebAssembly module
echo "🔨 Building WebAssembly module..."
wasm-pack build --target web --out-dir pkg --dev

# Copy the generated files to the website directory for GitHub Pages
echo "📁 Copying files to website directory..."
cp pkg/*.js website/
cp pkg/*.wasm website/

echo "✅ Build completed successfully!"
echo "📦 Generated files:"
echo "   - monkey_rs.js (JavaScript bindings)"
echo "   - monkey_rs_bg.wasm (WebAssembly binary)"
echo "   - monkey_rs.d.ts (TypeScript definitions)"

echo ""
echo "🚀 You can now open website/index.html in your browser to test the interpreter!"
echo "🌐 For GitHub Pages, commit and push these files to your repository."
