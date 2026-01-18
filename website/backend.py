#!/usr/bin/env python3
"""GLaDOS Playground Backend

A Flask server that executes GLaDOS code (LISP and new syntax).
"""

import os
import re
import subprocess
import sys
import tempfile


def _maybe_reexec_in_venv(missing_module: str) -> None:
    if missing_module not in {"flask", "flask_cors"}:
        return

    here = os.path.dirname(__file__)
    for venv_dir in (".venv", "venv"):
        candidate = os.path.join(here, venv_dir, "bin", "python")
        if os.path.exists(candidate) and os.path.abspath(sys.executable) != os.path.abspath(candidate):
            os.execv(candidate, [candidate] + sys.argv)


try:
    from flask import Flask, request, jsonify, send_from_directory
    from flask_cors import CORS
except ModuleNotFoundError as e:
    _maybe_reexec_in_venv(getattr(e, "name", ""))
    missing = getattr(e, "name", "<unknown>")
    sys.stderr.write(
        "Missing Python dependency: %s\n"
        "Tip: run 'cd website && python3 -m venv .venv && .venv/bin/pip install -r requirements.txt'\n"
        % missing
    )
    raise


ANSI_RE = re.compile(r"\x1B\[[0-?]*[ -/]*[@-~]")


def strip_ansi(text: str) -> str:
    return ANSI_RE.sub("", text)

app = Flask(__name__, static_folder='.')
CORS(app)

# Configuration
GLADOS_EXECUTABLE = os.path.join(os.path.dirname(__file__), '..', '.stack-work', 'install', 'x86_64-linux', '*', '9.4.8', 'bin', 'glados')
MAX_EXECUTION_TIME = 5
MAX_OUTPUT_SIZE = 10000

def find_glados():
    """Find the GLaDOS executable"""
    import glob
    patterns = [
        os.path.join(os.path.dirname(__file__), '..', 'glados'),
        os.path.join(os.path.dirname(__file__), '..', '.stack-work', 'install', '*', '*', '*', 'bin', 'glados'),
    ]
    for pattern in patterns:
        matches = glob.glob(pattern)
        if matches:
            return matches[0]
    return None

@app.route('/api/execute', methods=['POST'])
def execute_code():
    """Execute GLaDOS code (LISP syntax)"""
    return execute_with_extension('.scm')

@app.route('/api/execute-new', methods=['POST'])
def execute_new_code():
    """Execute GLaDOS code (new Python-like syntax)"""
    return execute_with_extension('.gla')

def execute_with_extension(extension):
    """Execute code with specified file extension"""
    try:
        data = request.get_json()
        code = data.get('code', '').strip()
        
        if not code:
            return jsonify({'success': False, 'error': 'No code provided'}), 400
        
        glados_path = find_glados()
        if not glados_path:
            # Try using stack exec
            glados_path = 'stack'
            use_stack = True
        else:
            use_stack = False
        
        with tempfile.NamedTemporaryFile(mode='w', suffix=extension, delete=False) as f:
            temp_file = f.name
            f.write(code)
        
        try:
            if use_stack:
                cmd = ['stack', 'exec', 'glados', '--', temp_file]
                cwd = os.path.join(os.path.dirname(__file__), '..')
            else:
                cmd = [glados_path, temp_file]
                cwd = None
            
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=MAX_EXECUTION_TIME,
                cwd=cwd
            )
            
            stdout = result.stdout[:MAX_OUTPUT_SIZE]
            stderr = result.stderr[:MAX_OUTPUT_SIZE]

            stdout = strip_ansi(stdout)
            stderr = strip_ansi(stderr)
            
            # Strip ANSI escape codes
            import re
            ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
            stdout = ansi_escape.sub('', stdout)
            stderr = ansi_escape.sub('', stderr)
            
            # Clean output
            lines = [l for l in stdout.split('\n') if l.strip() and 'Parsed' not in l and 'Evaluation' not in l]
            output = '\n'.join(lines)
            
            if result.returncode == 0:
                return jsonify({
                    'success': True,
                    'output': output or '(no output)',
                    'exit_code': result.returncode
                })
            else:
                return jsonify({
                    'success': False,
                    'error': stderr or output or 'Unknown error',
                    'exit_code': result.returncode
                })
                
        except subprocess.TimeoutExpired:
            return jsonify({
                'success': False,
                'error': f'Execution timed out after {MAX_EXECUTION_TIME} seconds'
            }), 408
        finally:
            try:
                os.unlink(temp_file)
            except:
                pass
                
    except Exception as e:
        return jsonify({
            'success': False,
            'error': f'Internal server error: {str(e)}'
        }), 500

@app.route('/api/health', methods=['GET'])
def health_check():
    """Health check endpoint"""
    glados_path = find_glados()
    return jsonify({
        'status': 'ok' if glados_path else 'using stack exec',
        'glados_path': glados_path or 'stack exec glados'
    })

@app.route('/api/examples', methods=['GET'])
def get_examples():
    """Return predefined examples for both syntaxes"""
    return jsonify({
        'lisp': [
            {
                'name': 'Hello World',
                'description': 'Simple arithmetic',
                'code': '(+ 21 21)'
            },
            {
                'name': 'Factorial',
                'description': 'Recursive factorial function',
                'code': '''(define (factorial n)
  (if (eq? n 1)
    1
    (* n (factorial (- n 1)))))

(factorial 10)'''
            },
            {
                'name': 'Fibonacci',
                'description': 'Recursive Fibonacci sequence',
                'code': '''(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 10)'''
            },
            {
                'name': 'Arithmetic',
                'description': 'Various math operations',
                'code': '''(+ 10 20)
(* 6 7)
(- 100 58)
(div 84 2)
(mod 10 3)'''
            },
            {
                'name': 'Conditionals',
                'description': 'If expressions',
                'code': '''(define x 42)
(if (> x 10)
  "greater"
  "smaller")'''
            }
        ],
        'new': [
            {
                'name': 'Hello World',
                'description': 'Simple arithmetic with infix operators',
                'code': '21 + 21'
            },
            {
                'name': 'Variables',
                'description': 'Define and use variables',
                'code': '''let x = 10
let y = 20
x + y * 2'''
            },
            {
                'name': 'Function',
                'description': 'Define and call a function',
                'code': '''def double(n): n * 2

double(21)'''
            },
            {
                'name': 'Factorial',
                'description': 'Recursive factorial with new syntax',
                'code': '''def factorial(n):
    if n == 1: 1 else: n * factorial(n - 1)

factorial(5)'''
            },
            {
                'name': 'Complex Math',
                'description': 'Operator precedence demo',
                'code': '''let a = 2 + 3 * 4
let b = (2 + 3) * 4
a + b'''
            },
            {
                'name': 'Conditionals',
                'description': 'If-else expressions',
                'code': '''let age = 25
if age >= 18: 100 else: 0'''
            },
            {
                'name': 'Lambda',
                'description': 'Anonymous functions',
                'code': '''let add = fn(a, b): a + b
add(10, 32)'''
            },
            {
                'name': 'Comparisons',
                'description': 'Comparison operators',
                'code': '''let x = 5
let y = 10
if x < y: 1 else: 0'''
            },
            {
                'name': 'Lists',
                'description': 'List literals, indexing, and append/set',
                'code': '''let xs = [1]
print(xs)

append(xs, 2)
print(xs[1])

set(xs, 0, 42)
print(xs)

for x in xs: print(x)'''
            }
        ]
    })

@app.route('/')
def serve_index():
    return send_from_directory('.', 'index.html')

@app.route('/<path:path>')
def serve_static(path):
    return send_from_directory('.', path)

if __name__ == '__main__':
    print("=" * 60)
    print("GLaDOS Playground Backend")
    print("=" * 60)
    glados_path = find_glados()
    print(f"GLaDOS: {glados_path or 'using stack exec'}")
    print("=" * 60)
    print("\nStarting server on http://localhost:5001")
    print("Endpoints:")
    print("  POST /api/execute     - Run LISP code")
    print("  POST /api/execute-new - Run new syntax code")
    print("  GET  /api/examples    - Get code examples")
    print("\nPress Ctrl+C to stop\n")
    
    app.run(host='0.0.0.0', port=5001, debug=False, threaded=True)
