import init, { MonkeyInterpreter, get_example_code, get_available_examples, set_output_callback } from './monkey_rs.js';

class MonkeyWebApp {
    constructor() {
        this.interpreter = null;
        this.isInitialized = false;
        this.init();
    }

    async init() {
        try {
            // Initialize the WebAssembly module
            await init();

            // Set up the output callback for real-time streaming
            this.setupOutputCallback();

            // Create a new interpreter instance
            this.interpreter = new MonkeyInterpreter();

            // Set up the UI
            this.setupEventListeners();
            this.populateExamples();

            this.isInitialized = true;
            console.log('Monkey WebAssembly interpreter initialized successfully!');
        } catch (error) {
            console.error('Failed to initialize WebAssembly module:', error);
            this.showError('Failed to initialize the Monkey interpreter. Please refresh the page.');
        }
    }

    setupOutputCallback() {
        // Create a callback function that will be called from WebAssembly
        const outputCallback = (text) => {
            // Use requestAnimationFr   ame to ensure the callback is processed asynchronously
            // and doesn't block the WebAssembly execution
            // requestAnimationFrame(() => {
            this.appendToOutput(text);
            // });
        };

        // Set the callback in the WebAssembly module
        set_output_callback(outputCallback);
    }

    setupEventListeners() {
        const runButton = document.getElementById('run-code');
        const loadExampleButton = document.getElementById('load-example');
        const clearButton = document.getElementById('clear-editor');
        const exampleSelector = document.getElementById('example-selector');

        runButton.addEventListener('click', () => this.runCode());
        loadExampleButton.addEventListener('click', () => this.loadExample());
        clearButton.addEventListener('click', () => this.clearEditor());
        exampleSelector.addEventListener('change', () => this.onExampleSelect());

        // Allow running code with Ctrl+Enter
        const codeEditor = document.getElementById('code-editor');
        codeEditor.addEventListener('keydown', (e) => {
            if (e.ctrlKey && e.key === 'Enter') {
                e.preventDefault();
                this.runCode();
            }
        });
    }

    populateExamples() {
        const exampleSelector = document.getElementById('example-selector');
        const examplesJson = get_available_examples();
        const examples = JSON.parse(examplesJson);

        // Clear existing options except the first one
        while (exampleSelector.children.length > 1) {
            exampleSelector.removeChild(exampleSelector.lastChild);
        }

        // Add example options
        examples.forEach(([value, label]) => {
            const option = document.createElement('option');
            option.value = value;
            option.textContent = label;
            exampleSelector.appendChild(option);
        });
    }

    onExampleSelect() {
        const selector = document.getElementById('example-selector');
        if (selector.value) {
            const loadButton = document.getElementById('load-example');
            loadButton.textContent = `Load ${selector.options[selector.selectedIndex].text}`;
        } else {
            const loadButton = document.getElementById('load-example');
            loadButton.textContent = 'Load Example';
        }
    }

    loadExample() {
        const selector = document.getElementById('example-selector');
        if (!selector.value) {
            this.showError('Please select an example first.');
            return;
        }

        const code = get_example_code(selector.value);
        const editor = document.getElementById('code-editor');
        editor.value = code;

        // Clear the selector
        selector.value = '';
        const loadButton = document.getElementById('load-example');
        loadButton.textContent = 'Load Example';
    }

    clearEditor() {
        const editor = document.getElementById('code-editor');
        editor.value = '';
        this.clearOutput();
    }

    async runCode() {
        if (!this.isInitialized) {
            this.showError('Interpreter not initialized yet. Please wait...');
            return;
        }

        const codeEditor = document.getElementById('code-editor');
        const code = codeEditor.value.trim();

        if (!code) {
            this.showError('Please enter some Monkey code to run.');
            return;
        }

        const runButton = document.getElementById('run-code');

        // Show loading state
        runButton.disabled = true;
        runButton.textContent = 'Running...';
        this.clearOutput();
        this.appendToOutput('Executing code...\n');

        try {
            const startTime = performance.now();

            // Set the program in the interpreter
            this.interpreter.set_program(code);

            // Step through statements asynchronously
            const step = async () => {
                // Evaluate one statement
                const result = this.interpreter.evaluate_statement();
                if (result === 'Program completed' || result === 'No program set') {
                    // Done
                    const endTime = performance.now();
                    const executionTime = (endTime - startTime).toFixed(2);
                    this.appendToOutput(`\n✓ Execution completed in ${executionTime}ms\n`);
                    runButton.disabled = false;
                    runButton.textContent = '▶ Run Code';
                    return;
                }
                // Yield to the event loop to allow DOM updates
                await new Promise(resolve => setTimeout(resolve, 0));
                // Continue to next statement
                step();
            };
            // Start stepping
            step();
        } catch (error) {
            console.error('Error executing code:', error);
            this.showError(`Execution error: ${error.message}`);
            runButton.disabled = false;
            runButton.textContent = '▶ Run Code';
        }
    }


    showError(message) {
        const output = document.getElementById('output');
        output.innerHTML = `<div class="error">❌ ${this.escapeHtml(message)}</div>`;
    }

    clearOutput() {
        const output = document.getElementById('output');
        output.innerHTML = '<div class="output-placeholder">Click "Run Code" to execute your Monkey program</div>';
    }

    appendToOutput(text) {
        // console.log('Appending to output:', text);
        const output = document.getElementById('output');

        // Remove placeholder if it exists
        const placeholder = output.querySelector('.output-placeholder');
        if (placeholder) {
            placeholder.remove();
        }

        // Create or get the output content div
        let contentDiv = output.querySelector('.output-content');
        if (!contentDiv) {
            contentDiv = document.createElement('div');
            contentDiv.className = 'output-content';
            contentDiv.style.fontFamily = 'JetBrains Mono, Fira Code, Consolas, monospace';
            contentDiv.style.fontSize = '14px';
            contentDiv.style.lineHeight = '1.5';
            contentDiv.style.whiteSpace = 'pre-wrap';
            output.appendChild(contentDiv);
        }

        // Append the text as-is (newlines are handled by WebAssembly)
        contentDiv.textContent += text;

        // Scroll to bottom
        output.scrollTop = output.scrollHeight;
    }

    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }
}

// Initialize the app when the DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new MonkeyWebApp();
});

// Add some helpful keyboard shortcuts info
document.addEventListener('DOMContentLoaded', () => {
    const codeEditor = document.getElementById('code-editor');
    codeEditor.placeholder = 'Enter your Monkey code here...\n\nTip: Use Ctrl+Enter to run your code quickly!';
});
