# Monkey Language — Web Interface

Interactive web playground for the Monkey programming language, powered by Rust and WebAssembly.

## Files

- `index.html` — Single-page app with code editor, output display, and engine toggle
- `styles.css` — Dark theme styling with glassmorphism
- `app.js` — Unified JavaScript: async VM stepping, tree-walker fallback, stop button
- `monkey_rs.js` — Generated WebAssembly JavaScript bindings
- `monkey_rs_bg.wasm` — Compiled WebAssembly binary

## Architecture

The website supports two execution engines:

- **VM (Bytecode)** — Compiles Monkey code to bytecode and executes it instruction-by-instruction via the `step()` API. JS calls `step()` in batches of 500, yielding to the browser event loop between batches so the DOM can paint. This enables **progressive output** for long-running programs.
- **Tree-Walker** — Evaluates the AST directly via `evaluate()`. Blocks the browser until completion — fine for short programs but freezes the UI for long-running ones.

## Local Development

1. Build the WebAssembly module from the project root:
   ```bash
   just build_website
   ```

2. Start a local HTTP server:
   ```bash
   just serve
   ```

3. Open your browser to: http://localhost:8000/
