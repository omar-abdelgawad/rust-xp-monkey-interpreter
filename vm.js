import init, { MonkeyVM, get_example_code, get_available_examples } from './monkey_rs.js';

class MonkeyWebApp {
    constructor() {
        this.vm = null;
        this.isInitialized = false;
        this.init();
    }

    async init() {
        try {
            await init();

            // Create VM instead of Interpreter
            this.vm = new MonkeyVM();

            this.setupEventListeners();
            this.populateExamples();

            this.isInitialized = true;
            console.log('Monkey VM initialized successfully!');
        } catch (error) {
            console.error(error);
            this.showError('Failed to initialize the Monkey VM. Please refresh.');
        }
    }

    setupEventListeners() {
        const runButton = document.getElementById('run-code');
        const clearButton = document.getElementById('clear-editor');
        const exampleSelector = document.getElementById('example-selector');

        runButton.addEventListener('click', () => this.runCode());
        clearButton.addEventListener('click', () => this.clearEditor());
        exampleSelector.addEventListener('change', () => this.loadExample());

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
        const examples = JSON.parse(get_available_examples());

        while (exampleSelector.children.length > 1) {
            exampleSelector.removeChild(exampleSelector.lastChild);
        }

        examples.forEach(([value, label]) => {
            const option = document.createElement('option');
            option.value = value;
            option.textContent = label;
            exampleSelector.appendChild(option);
        });
    }

    loadExample() {
        const selector = document.getElementById('example-selector');
        if (!selector.value) return;

        const code = get_example_code(selector.value);
        document.getElementById('code-editor').value = code;
    }

    clearEditor() {
        document.getElementById('code-editor').value = '';
        this.clearOutput();
        document.getElementById('example-selector').value = '';
    }

    runCode() {
        if (!this.isInitialized) {
            this.showError('VM not initialized yet.');
            return;
        }

        const runButton = document.getElementById('run-code');
        const code = document.getElementById('code-editor').value.trim();

        if (!code) {
            this.showError('Please enter some Monkey code.');
            return;
        }

        runButton.disabled = true;
        runButton.textContent = 'Running...';

        this.clearOutput();

        try {
            const start = performance.now();

            // 🔥 This is now ONE CALL
            const result = this.vm.evaluate(code);

            const end = performance.now();
            const duration = (end - start).toFixed(2);

            this.appendToOutput(result);
            this.appendToOutput(`\n\n✓ Execution completed in ${duration} ms`);

        } catch (error) {
            this.showError(`Execution error: ${error.message}`);
        }

        runButton.disabled = false;
        runButton.textContent = '▶ Run Code';
    }

    showError(message) {
        const output = document.getElementById('output');
        output.innerHTML = `<div class="error">❌ ${this.escapeHtml(message)}</div>`;
    }

    clearOutput() {
        const output = document.getElementById('output');
        output.innerHTML =
            '<div class="output-placeholder">Click "Run Code" to execute your Monkey program</div>';
    }

    appendToOutput(text) {
        const output = document.getElementById('output');

        const placeholder = output.querySelector('.output-placeholder');
        if (placeholder) placeholder.remove();

        let contentDiv = output.querySelector('.output-content');
        if (!contentDiv) {
            contentDiv = document.createElement('div');
            contentDiv.className = 'output-content';
            contentDiv.style.fontFamily = 'JetBrains Mono, monospace';
            contentDiv.style.whiteSpace = 'pre-wrap';
            output.appendChild(contentDiv);
        }

        contentDiv.textContent += text;
        output.scrollTop = output.scrollHeight;
    }

    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }
}

document.addEventListener('DOMContentLoaded', () => {
    new MonkeyWebApp();
});
