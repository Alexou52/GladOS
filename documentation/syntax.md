# GLaDOS Syntax Guide

## S-Expressions

GLaDOS uses S-expressions (symbolic expressions) as its fundamental syntax. An S-expression is either:

1. An **atom** (integer, boolean, or symbol)
2. A **list** of S-expressions enclosed in parentheses

### Atoms

#### Integers
```scheme
42          ; positive integer
-17         ; negative integer
0           ; zero
```

#### Booleans
```scheme
#t          ; true
#f          ; false
```

#### Symbols
```scheme
foo         ; simple symbol
+           ; operator symbols
my-var      ; symbols with hyphens
eq?         ; symbols with special chars
```

## Special Forms

Special forms are syntactic constructs with special evaluation rules.

### Define

Bind a value to a symbol:

```scheme
(define x 42)
(define name "GLaDOS")
```

Define a function (syntactic sugar for lambda):

```scheme
; These two are equivalent:
(define (add a b) (+ a b))
(define add (lambda (a b) (+ a b)))
```

### Lambda

Create anonymous functions:

```scheme
(lambda (x) (* x 2))           ; single parameter
(lambda (a b) (+ a b))         ; multiple parameters
(lambda () 42)                 ; no parameters
```

Example with lambda:

```scheme
; Store lambda in a variable
(define double (lambda (x) (* x 2)))
(double 21)  ; => 42

; Use lambda directly
((lambda (x y) (+ x y)) 10 20)  ; => 30
```

### If

Conditional expressions:

```scheme
(if condition then-expr else-expr)
```

The condition must evaluate to a boolean:

```scheme
(if #t 1 2)           ; => 1
(if #f 1 2)           ; => 2
(if (< 5 10) "yes" "no")  ; => "yes"
```

**Important**: Both branches are expressions, not statements. The if form returns a value.

## Function Calls

A function call is a list where the first element is the function and the rest are arguments:

```scheme
(function arg1 arg2 arg3)
```

Examples:

```scheme
(+ 1 2 3)              ; => 6
(* 4 5)                ; => 20
(eq? 10 10)            ; => #t
```

Nested calls:

```scheme
(+ (* 2 3) (- 10 5))   ; => 11
; Equivalent to: (2 * 3) + (10 - 5)
```

## Recursion

Functions can call themselves:

```scheme
(define (countdown n)
  (if (eq? n 0)
    "Done!"
    (countdown (- n 1))))

(countdown 5)  ; => "Done!"
```

### Tail Recursion

GLaDOS optimizes tail-recursive functions:

```scheme
; Tail-recursive factorial
(define (fact n acc)
  (if (eq? n 0)
    acc
    (fact (- n 1) (* n acc))))

(define (factorial n)
  (fact n 1))

(factorial 1000)  ; No stack overflow!
```

## Comments

Single-line comments start with semicolon:

```scheme
; This is a comment
(define x 42)  ; inline comment
```

## Whitespace

GLaDOS is whitespace-insensitive. These are equivalent:

```scheme
(+ 1 2)

(+
  1
  2)

(+    1    2)
```

## Operator Precedence

GLaDOS uses prefix notation (Polish notation), so there's no operator precedence to remember!

```scheme
; In GLaDOS (prefix):
(+ (* 2 3) 4)

; Equivalent in infix notation:
; (2 * 3) + 4
```

## Examples

### Define multiple values

```scheme
(define pi 3.14159)
(define radius 5)
(define area (* pi (* radius radius)))
area  ; => 78.53975
```

### Nested functions

```scheme
(define (outer x)
  (define (inner y)
    (+ x y))
  (inner 10))

(outer 5)  ; => 15
```

### Higher-order functions

```scheme
(define (apply-twice f x)
  (f (f x)))

(define (add1 x) (+ x 1))

(apply-twice add1 10)  ; => 12
```

### Conditional logic

```scheme
(define (abs x)
  (if (< x 0)
    (- 0 x)
    x))

(abs -42)  ; => 42
(abs 42)   ; => 42
```

## Best Practices

1. **Use meaningful names** for functions and variables
2. **Keep functions small** and focused on one task
3. **Prefer recursion** over loops (not available anyway!)
4. **Use tail recursion** for functions that might recurse deeply
5. **Add comments** to explain complex logic

## Common Patterns

### Guard pattern

```scheme
(define (sign x)
  (if (< x 0)
    -1
    (if (eq? x 0)
      0
      1)))
```

### Accumulator pattern

```scheme
(define (sum-list lst acc)
  (if (null? lst)
    acc
    (sum-list (cdr lst) (+ acc (car lst)))))
```

---

# Part 2: New Syntax (GLaDOS Language)

GLaDOS also supports a modern, Python-like syntax that compiles to bytecode and runs on a virtual machine.

## File Extensions

- `.scm` - LISP/Scheme syntax (interpreted)
- `.gla` - GLaDOS new syntax (compiled to bytecode)

## Basic Expressions

### Arithmetic with Infix Operators

```glados
21 + 21       // Result: 42
10 - 3        // Result: 7
6 * 7         // Result: 42
15 / 3        // Result: 5
17 % 5        // Result: 2
```

### Operator Precedence

```glados
2 + 3 * 4     // Result: 14 (multiplication first)
(2 + 3) * 4   // Result: 20 (parentheses override)
```

## Variables

Use `let` to define variables:

```glados
let x = 42
let name = "GLaDOS"
let result = 10 + 5
```

## Lists

List literals use square brackets:

```glados
let xs = [1, 2, 3]
let empty = []
```

### Indexing

```glados
let xs = [10, 20, 30]
print(xs[0])   // 10
print(xs[1])   // 20
```

Indexing is 0-based. Out-of-bounds indexes raise a runtime error.

### Mutating a list (Python-like)

`append` and `set` update the list variable in-place (you don't need to reassign):

```glados
let xs = [1]
append(xs, 2)
print(xs)      // [1, 2]

set(xs, 0, 42)
print(xs)      // [42, 2]
```

### Concatenation

```glados
print(concat([1, 2], [3, 4]))  // [1, 2, 3, 4]
```

### Loops over lists

```glados
let xs = [1, 2, 3]

for x in xs:
  print(x)
```

### Truthiness

In conditions, an empty list is false and a non-empty list is true:

```glados
if []: 1 else: 0      // 0
if [1]: 1 else: 0     // 1
```

## Functions

### Function Definition

Use `def` to define named functions:

```glados
def add(a, b):
    a + b

def factorial(n):
    if n <= 1:
        1
    else:
        n * factorial(n - 1)
```

### Lambda Functions

Anonymous functions with `fn`:

```glados
let double = fn(x): x * 2
let add = fn(a, b): a + b
```

### Function Calls

```glados
add(2, 3)           // Result: 5
factorial(5)        // Result: 120
double(21)          // Result: 42
```

## Control Flow

### Conditionals

```glados
if x > 0:
    "positive"
else:
    "non-positive"
```

### Comparison Operators

```glados
x == y    // Equal
x != y    // Not equal (future)
x < y     // Less than
x > y     // Greater than
x <= y    // Less or equal
x >= y    // Greater or equal
```

## Complete Example

```glados
// Define a function
def double(n):
    n * 2

// Define variables
let x = 5
let y = double(x)

// Compute final result
x * 2 + y    // Result: 20
```

## Bytecode Compilation

The new syntax is compiled to bytecode instructions:

| Instruction | Description |
|-------------|-------------|
| `PUSH val`  | Push value on stack |
| `POP`       | Pop top of stack |
| `ADD`       | Add top two values |
| `SUB`       | Subtract |
| `MUL`       | Multiply |
| `DIV`       | Divide |
| `MOD`       | Modulo |
| `LOAD var`  | Load variable |
| `STORE var` | Store variable |
| `CALL n`    | Call function with n args |
| `RET`       | Return from function |
| `HALT`      | Stop execution |

---

Next: [Built-in Functions →](builtins.md)
