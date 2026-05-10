import init, {
    MonkeyVM,
    get_example_code,
    get_available_examples,
    set_output_callback
} from './monkey_rs.js';

const BATCH_SIZE = 500;

class MonkeyWebApp {
    constructor() {
        this.vm = null;
        this.isRunning = false;
        this.stepHandle = null;
        this.isInitialized = false;
        this.init();
    }

    async init() {
        try {
            await init();

            set_output_callback((text) => {
                this.appendToOutput(text);
            });

            this.vm = new MonkeyVM();

            this.setupEventListeners();
            this.populateExamples();

            this.isInitialized = true;
            document.body.classList.add('loaded');
        } catch (error) {
            console.error('Failed to initialize:', error);
            this.showError('Failed to initialize. Please refresh the page.');
        }
    }

    setupEventListeners() {
        document.getElementById('run-code').addEventListener('click', () => this.runCode());
        document.getElementById('stop-code').addEventListener('click', () => this.stopExecution());
        document.getElementById('clear-editor').addEventListener('click', () => this.clearEditor());
        document.getElementById('example-selector').addEventListener('change', () => this.loadExample());

        // Ctrl+Enter
        document.getElementById('code-editor').addEventListener('keydown', (e) => {
            if (e.ctrlKey && e.key === 'Enter') {
                e.preventDefault();
                this.runCode();
            }
            // Tab key inserts spaces
            if (e.key === 'Tab') {
                e.preventDefault();
                const ta = e.target;
                const start = ta.selectionStart;
                const end = ta.selectionEnd;
                ta.value = ta.value.substring(0, start) + '    ' + ta.value.substring(end);
                ta.selectionStart = ta.selectionEnd = start + 4;
            }
        });
    }

    populateExamples() {
        const selector = document.getElementById('example-selector');
        const examples = JSON.parse(get_available_examples());
        while (selector.children.length > 1) {
            selector.removeChild(selector.lastChild);
        }
        examples.forEach(([value, label]) => {
            const option = document.createElement('option');
            option.value = value;
            option.textContent = label;
            selector.appendChild(option);
        });
    }

    loadExample() {
        const selector = document.getElementById('example-selector');
        if (!selector.value) return;
        document.getElementById('code-editor').value = get_example_code(selector.value);
    }

    clearEditor() {
        document.getElementById('code-editor').value = '';
        this.clearOutput();
        this.clearCompilerOutput();
        document.getElementById('example-selector').value = '';
    }

    runCode() {
        if (!this.isInitialized) {
            this.showError('Not initialized yet. Please wait...');
            return;
        }
        if (this.isRunning) return;

        const code = document.getElementById('code-editor').value.trim();
        if (!code) {
            this.showError('Please enter some Monkey code to run.');
            return;
        }

        this.clearOutput();
        this.clearCompilerOutput();
        this.setRunningState(true);

        this.runCodeVM(code);
    }

    runCodeVM(code) {
        const startTime = performance.now();

        const success = this.vm.compile(code);
        if (!success) {
            this.setRunningState(false);
            return;
        }

        const instructions = this.vm.get_instructions();
        this.updateCompilerOutput(instructions);

        const stepBatch = () => {
            if (!this.isRunning) {
                this.appendStatus('⏹ Execution stopped');
                this.setRunningState(false);
                return;
            }

            for (let i = 0; i < BATCH_SIZE; i++) {
                if (!this.vm.is_running()) {
                    const duration = (performance.now() - startTime).toFixed(2);
                    const result = this.vm.result();
                    if (result && result !== 'null') {
                        this.appendToOutput(result + '\n');
                    }
                    this.appendStatus(`✓ Completed in ${duration}ms`);
                    this.setRunningState(false);
                    return;
                }
                if (!this.vm.step()) {
                    this.setRunningState(false);
                    return;
                }
            }
            this.stepHandle = setTimeout(stepBatch, 0);
        };

        stepBatch();
    }

    stopExecution() {
        if (!this.isRunning) return;
        this.isRunning = false;
        if (this.stepHandle) {
            clearTimeout(this.stepHandle);
            this.stepHandle = null;
        }
        this.appendStatus('⏹ Execution stopped');
        this.setRunningState(false);
    }

    setRunningState(running) {
        this.isRunning = running;
        const runBtn = document.getElementById('run-code');
        const stopBtn = document.getElementById('stop-code');

        if (running) {
            runBtn.style.display = 'none';
            stopBtn.style.display = 'inline-flex';
        } else {
            runBtn.style.display = 'inline-flex';
            stopBtn.style.display = 'none';
        }
    }

    showError(message) {
        const output = document.getElementById('output');
        output.innerHTML = `<div class="error-msg">${this.escapeHtml(message)}</div>`;
    }

    clearOutput() {
        const output = document.getElementById('output');
        output.innerHTML = '<div class="output-placeholder">Output will appear here...</div>';
    }

    clearCompilerOutput() {
        const compilerOutput = document.getElementById('compiler-output');
        compilerOutput.innerHTML = '<div class="output-placeholder">Bytecode instructions will appear here...</div>';
    }

    updateCompilerOutput(text) {
        const compilerOutput = document.getElementById('compiler-output');
        compilerOutput.innerHTML = '';
        
        const contentDiv = document.createElement('div');
        contentDiv.className = 'output-content';
        contentDiv.textContent = text;
        compilerOutput.appendChild(contentDiv);
    }

    appendToOutput(text) {
        const output = document.getElementById('output');
        const placeholder = output.querySelector('.output-placeholder');
        if (placeholder) placeholder.remove();

        let contentDiv = output.querySelector('.output-content');
        if (!contentDiv) {
            contentDiv = document.createElement('div');
            contentDiv.className = 'output-content';
            output.appendChild(contentDiv);
        }

        contentDiv.textContent += text;
        output.scrollTop = output.scrollHeight;
    }

    appendStatus(text) {
        const output = document.getElementById('output');
        const statusDiv = document.createElement('div');
        statusDiv.className = 'output-status';
        statusDiv.textContent = text;
        output.appendChild(statusDiv);
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
