# GLaDOS - A Lisp-like Language Interpreter

[![CI/CD Pipeline](https://github.com/EpitechPGE3-2025/G-FUN-500-NCE-5-2-glados-5/actions/workflows/ci.yml/badge.svg)](https://github.com/EpitechPGE3-2025/G-FUN-500-NCE-5-2-glados-5/actions/workflows/ci.yml)

## Description

GLaDOS is a Lisp-like programming language interpreter written in Haskell. It implements a complete pipeline from parsing to evaluation of Scheme-like expressions.

## Features

- **S-Expression Parsing**: Parse Lisp-style symbolic expressions
- **Abstract Syntax Tree (AST)**: Convert syntax to semantic representation
- **Evaluation**: Execute arithmetic, logical, and conditional operations
- **Type Safety**: Uses Maybe monad for safe error handling
- **Comprehensive Testing**: Unit and integration tests with coverage reporting

## Building

### Prerequisites

- GHC 9.4.7 or later
- Stack (recommended build tool)

### Installation

```bash
# Install dependencies
make install

# Build the project
make

# This will create a binary named 'glados'
```

### Makefile Targets

- `make` - Build the project
- `make clean` - Clean build artifacts
- `make fclean` - Full clean (removes binary)
- `make re` - Rebuild from scratch
- `make test` - Run all tests with coverage
- `make coverage` - Generate detailed coverage report
- `make run` - Build and run the binary
- `make install` - Install dependencies

## Usage

```bash
./glados [file]
```

## Testing

The project includes comprehensive unit and integration tests using HSpec.

```bash
# Run all tests
make test

# Run tests with coverage report
make coverage

# Watch mode for development
make test-watch
```

### Test Coverage

Tests cover:
- S-Expression data structures and operations
- Parser functionality (SExpr → AST conversion)
- Evaluation of arithmetic, logical, and conditional expressions
- Error handling and edge cases

## Continuous Integration / Continuous Delivery

The project uses GitHub Actions for automated testing and building:

- **CI Pipeline**: Runs on every push and pull request
  - Builds the project
  - Runs all tests with coverage
  - Generates coverage reports
  - Validates binary creation

- **CD Pipeline**: Creates release builds on main branch
  - Produces a fully functional executable
  - Archives release artifacts
  - Retains builds for 30 days

- **Code Quality**: Runs linting checks
  - Uses HLint for code quality analysis

## Project Structure

```
glados/
├── src/               # Source files
│   ├── Main.hs       # Entry point
│   ├── SExpr.hs      # Symbolic expressions
│   ├── AST.hs        # Abstract syntax tree
│   ├── Parser.hs     # Parser (SExpr → AST)
│   └── Eval.hs       # Evaluator
├── test/             # Test files
│   ├── Spec.hs       # Test entry point
│   ├── SExprSpec.hs  # SExpr tests
│   ├── ParserSpec.hs # Parser tests
│   └── EvalSpec.hs   # Evaluator tests
├── Makefile          # Build automation
├── glados.cabal      # Cabal configuration
└── stack.yaml        # Stack configuration
```

## Language Features

### Supported Operations

#### Arithmetic
- `(+ a b ...)` - Addition
- `(- a b ...)` - Subtraction
- `(* a b ...)` - Multiplication
- `(/ a b)` - Division

#### Comparison
- `(> a b)` - Greater than
- `(< a b)` - Less than
- `(>= a b)` - Greater or equal
- `(<= a b)` - Less or equal
- `(== a b)` - Equal
- `(!= a b)` - Not equal

#### Control Flow
- `(if condition then else)` - Conditional expression

#### Special Forms
- `(define name value)` - Variable definition

### Examples

```scheme
; Simple arithmetic
(* (+ 4 3) 6)  ; Returns 42

; Nested operations
(+ 2 (* 3 4))  ; Returns 14

; Conditionals
(if (> 10 5) (* 2 3) (+ 1 1))  ; Returns 6

; Variable definition
(define x 5)
```

## Error Handling

The interpreter uses `Maybe` types for safe error handling:
- Division by zero returns `Nothing`
- Invalid syntax returns `Nothing`
- Unknown functions return `Nothing`
- Type mismatches return `Nothing`

## Development

### Code Quality

The project follows Haskell best practices:
- No unsafe functions
- No mutable constructs
- Pure functional approach
- Comprehensive type safety

### Contributing

1. Create a feature branch
2. Make your changes
3. Ensure all tests pass: `make test`
4. Submit a pull request

The CI pipeline will automatically:
- Build your changes
- Run all tests
- Check code quality

## License

EPITECH PROJECT, 2025

## Authors

Epitech Students - PGE3 2025
