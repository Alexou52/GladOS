# Introduction to GLaDOS

## What is GLaDOS?

**GLaDOS** (Generic Language and Data Operand Syntax) is a modern functional programming language inspired by LISP and built with Haskell. It combines the elegance of symbolic expressions with powerful functional programming features.

## Philosophy

GLaDOS is designed with these core principles:

### 🎯 Functional First
Everything in GLaDOS is an expression. Functions are first-class citizens, and immutability is the default.

### 🔒 Type Safety
Built on Haskell's strong type system, GLaDOS ensures reliability and catches errors at compile time.

### 🚀 Performance
With a custom bytecode compiler and virtual machine, GLaDOS delivers efficient execution while maintaining expressiveness.

### 🧩 Simplicity
S-expressions provide a uniform syntax that's easy to parse, manipulate, and reason about.

## Key Features

- **First-class functions and closures**
- **Tail call optimization**
- **Pattern matching**
- **Comprehensive standard library**
- **Interactive REPL**
- **Extensive test coverage**

## Who is GLaDOS for?

GLaDOS is perfect for:

- **Functional programming enthusiasts** wanting a LISP-like language
- **Students** learning programming language design
- **Researchers** experimenting with language features
- **Developers** seeking a simple yet powerful scripting language

## Getting Started

The quickest way to get started is:

```bash
git clone https://github.com/your-repo/glados
cd glados
make
./glados
```

## Two Syntaxes, One Language

GLaDOS supports **two syntax modes**:

### LISP Mode (`.scm` files)
The classic S-expression syntax, interpreted directly:

```scheme
(define (factorial n)
  (if (eq? n 0)
    1
    (* n (factorial (- n 1)))))

(factorial 5)  ; => 120
```

### New Syntax Mode (`.gla` files)
A modern, Python-like syntax compiled to bytecode:

```glados
def factorial(n):
    if n <= 1:
        1
    else:
        n * factorial(n - 1)

factorial(5)  // => 120
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      GLaDOS                             │
├────────────────────────┬────────────────────────────────┤
│     LISP Mode (.scm)   │     New Syntax Mode (.gla)     │
├────────────────────────┼────────────────────────────────┤
│    S-Expression Parser │     Lexer → NewParser          │
│           ↓            │           ↓                    │
│         AST            │         AST                    │
│           ↓            │           ↓                    │
│    Direct Interpreter  │     Compiler → Bytecode        │
│                        │           ↓                    │
│                        │    Virtual Machine (VM)        │
├────────────────────────┴────────────────────────────────┤
│                      Result                             │
└─────────────────────────────────────────────────────────┘
```

## Example: Hello World

```scheme
; Define a greeting function
(define (greet name)
  (+ "Hello, " name "!"))

; Call the function
(greet "World")
; => "Hello, World!"
```

## Example: Factorial (New Syntax)

```glados
def factorial(n):
    if n <= 1:
        1
    else:
        n * factorial(n - 1)

factorial(5)  // => 120
```

## Next Steps

- Read the [Syntax Guide](syntax.md) to learn both syntaxes
- Explore [Built-in Functions](builtins.md) for available operations
- Check out [Examples](examples.md) for real-world code patterns
- Try the [Playground](/playground.html) to test code interactively

## Authors

GLaDOS is developed and maintained by:

- **Alexandre LEROUX-DRAGONI** - [alexandre.leroux-dragoni@epitech.eu](mailto:alexandre.leroux-dragoni@epitech.eu)
- **Adam LUBREZ** - [adam.lubrez@epitech.eu](mailto:adam.lubrez@epitech.eu)
- **Amine SADIK** - [amine.sadik@epitech.eu](mailto:amine.sadik@epitech.eu)
- **Nathan DERENTY** - [nathan.derenty@epitech.eu](mailto:nathan.derenty@epitech.eu)

---

*GLaDOS: Making functional programming accessible and fun!* 🎉
