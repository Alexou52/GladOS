# Built-in Functions

GLaDOS provides a comprehensive set of built-in functions for common operations.

## Arithmetic Operations

### Addition: `+`

```scheme
(+ 1 2)        ; => 3
(+ 1 2 3 4)    ; => 10
```

**Parameters**: Two or more integers  
**Returns**: Sum of all arguments

### Subtraction: `-`

```scheme
(- 10 3)       ; => 7
(- 10 3 2)     ; => 5
```

**Parameters**: Two or more integers  
**Returns**: First argument minus all subsequent arguments

### Multiplication: `*`

```scheme
(* 2 3)        ; => 6
(* 2 3 4)      ; => 24
```

**Parameters**: Two or more integers  
**Returns**: Product of all arguments

### Division: `div`

```scheme
(div 10 2)     ; => 5
(div 15 4)     ; => 3  (integer division)
```

**Parameters**: Two integers (divisor cannot be 0)  
**Returns**: Integer quotient  
**Error**: Division by zero raises an error

### Modulo: `mod`

```scheme
(mod 10 3)     ; => 1
(mod 15 4)     ; => 3
```

**Parameters**: Two integers (divisor cannot be 0)  
**Returns**: Remainder of division  
**Error**: Modulo by zero raises an error

## Comparison Operations

### Equality: `eq?`

```scheme
(eq? 5 5)      ; => #t
(eq? 5 6)      ; => #f
(eq? #t #t)    ; => #t
```

**Parameters**: Two values of the same type  
**Returns**: `#t` if equal, `#f` otherwise  
**Works with**: Integers and booleans

### Less Than: `<`

```scheme
(< 5 10)       ; => #t
(< 10 5)       ; => #f
(< 5 5)        ; => #f
```

**Parameters**: Two integers  
**Returns**: `#t` if first is strictly less than second

## Examples

### Complex Arithmetic

```scheme
; Calculate (2 + 3) * 4
(* (+ 2 3) 4)  ; => 20

; Calculate 10 / 2 + 3
(+ (div 10 2) 3)  ; => 8
```

### Building Other Comparisons

You can build other comparison operators using the built-ins:

```scheme
; Greater than
(define (> a b)
  (if (eq? a b)
    #f
    (if (< a b)
      #f
      #t)))

(> 10 5)  ; => #t

; Less than or equal
(define (<= a b)
  (if (< a b)
    #t
    (eq? a b)))

(<= 5 5)  ; => #t
(<= 5 6)  ; => #t

; Not equal
(define (!= a b)
  (if (eq? a b)
    #f
    #t))

(!= 5 6)  ; => #t
```

### Practical Examples

#### Absolute value

```scheme
(define (abs x)
  (if (< x 0)
    (- 0 x)
    x))

(abs -42)  ; => 42
```

#### Even/odd checking

```scheme
(define (even? n)
  (eq? (mod n 2) 0))

(define (odd? n)
  (eq? (mod n 2) 1))

(even? 4)  ; => #t
(odd? 7)   ; => #t
```

#### Power function

```scheme
(define (pow base exp)
  (if (eq? exp 0)
    1
    (* base (pow base (- exp 1)))))

(pow 2 10)  ; => 1024
```

#### GCD (Greatest Common Divisor)

```scheme
(define (gcd a b)
  (if (eq? b 0)
    a
    (gcd b (mod a b))))

(gcd 48 18)  ; => 6
```

#### Fibonacci

```scheme
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(fib 10)  ; => 55
```

#### Prime checking

```scheme
(define (is-divisible? n d)
  (eq? (mod n d) 0))

(define (is-prime-helper n d)
  (if (eq? d 1)
    #t
    (if (is-divisible? n d)
      #f
      (is-prime-helper n (- d 1)))))

(define (is-prime n)
  (if (< n 2)
    #f
    (is-prime-helper n (- n 1))))

(is-prime 17)  ; => #t
(is-prime 18)  ; => #f
```

## Error Handling

Built-in functions will raise errors in these cases:

### Division by Zero

```scheme
(div 10 0)   ; *** ERROR: division by zero
(mod 5 0)    ; *** ERROR: division by zero
```

### Type Mismatches

```scheme
(+ 5 #t)     ; *** ERROR: type mismatch
(< 5 #f)     ; *** ERROR: type mismatch
```

### Unbound Variables

```scheme
(+ x 5)      ; *** ERROR: variable x is not bound
```

## Tips and Tricks

### 1. Chaining Comparisons

```scheme
(define (between? x low high)
  (if (< x low)
    #f
    (if (< x high)
      #t
      #f)))

(between? 5 0 10)  ; => #t
```

### 2. Boolean Algebra

```scheme
; AND
(define (and a b)
  (if a b #f))

; OR
(define (or a b)
  (if a #t b))

---

# New Syntax Built-ins (.gla)

The Python-like syntax shares the same runtime, but exposes a few built-ins that are especially useful with lists.

## `len(x)`

```glados
print(len([1, 2, 3]))   // 3
```

Works on lists and strings.

## `get(x, i)` and `x[i]`

```glados
let xs = [10, 20, 30]
print(get(xs, 1))   // 20
print(xs[1])        // 20
```

## `append(list, value)`

Appends to the list variable (Python-like mutation):

```glados
let xs = [1]
append(xs, 2)
print(xs)           // [1, 2]
```

## `set(list, index, value)`

Replaces an element by index (Python-like mutation):

```glados
let xs = [1, 2, 3]
set(xs, 1, 99)
print(xs)           // [1, 99, 3]
```

## `concat(listA, listB)`

```glados
print(concat([1, 2], [3, 4]))  // [1, 2, 3, 4]
```

; NOT
(define (not x)
  (if x #f #t))
```

### 3. Min/Max Functions

```scheme
(define (min a b)
  (if (< a b) a b))

(define (max a b)
  (if (< a b) b a))
```

### 4. Sign Function

```scheme
(define (sign x)
  (if (< x 0)
    -1
    (if (eq? x 0)
      0
      1)))
```

---

Next: [Examples →](examples.md)
