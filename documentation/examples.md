# GLaDOS Examples

A collection of practical examples demonstrating GLaDOS features and patterns.

## Basic Examples

### Hello World

```scheme
; Simple value definition
(define message "Hello, GLaDOS!")
message  ; => "Hello, GLaDOS!"
```

### Simple Arithmetic

```scheme
; Basic math operations
(+ 2 3)           ; => 5
(* 4 5)           ; => 20
(div 10 2)        ; => 5
(mod 7 3)         ; => 1

; Nested expressions
(+ (* 2 3) (div 10 2))  ; => 11
```

## Recursive Functions

### Factorial

```scheme
; Classic recursive factorial
(define (factorial n)
  (if (eq? n 0)
    1
    (* n (factorial (- n 1)))))

(factorial 5)   ; => 120
(factorial 10)  ; => 3628800
```

### Tail-Recursive Factorial

```scheme
; More efficient with tail recursion
(define (fact-helper n acc)
  (if (eq? n 0)
    acc
    (fact-helper (- n 1) (* n acc))))

(define (factorial n)
  (fact-helper n 1))

(factorial 5)  ; => 120
```

### Fibonacci Sequence

```scheme
; Naive recursive fibonacci
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(fib 0)   ; => 0
(fib 1)   ; => 1
(fib 10)  ; => 55
```

### Optimized Fibonacci

```scheme
; Tail-recursive fibonacci
(define (fib-helper n a b)
  (if (eq? n 0)
    a
    (fib-helper (- n 1) b (+ a b))))

(define (fibonacci n)
  (fib-helper n 0 1))

(fibonacci 10)  ; => 55
```

## Arithmetic Functions

### Power Function

```scheme
(define (pow base exp)
  (if (eq? exp 0)
    1
    (* base (pow base (- exp 1)))))

(pow 2 8)   ; => 256
(pow 3 4)   ; => 81
```

### GCD (Euclidean Algorithm)

```scheme
(define (gcd a b)
  (if (eq? b 0)
    a
    (gcd b (mod a b))))

(gcd 48 18)   ; => 6
(gcd 100 35)  ; => 5
```

### LCM

```scheme
(define (lcm a b)
  (div (* a b) (gcd a b)))

(lcm 12 18)  ; => 36
```

## Comparison Functions

### Greater Than

```scheme
(define (> a b)
  (if (eq? a b)
    #f
    (if (< a b)
      #f
      #t)))

(> 10 5)  ; => #t
(> 5 10)  ; => #f
```

### Greater Than or Equal

```scheme
(define (>= a b)
  (if (> a b)
    #t
    (eq? a b)))

(>= 10 10)  ; => #t
(>= 10 5)   ; => #t
```

### Between Check

```scheme
(define (between? x low high)
  (if (< x low)
    #f
    (if (< high x)
      #f
      #t)))

(between? 5 0 10)   ; => #t
(between? 15 0 10)  ; => #f
```

## Boolean Logic

### AND

```scheme
(define (and a b)
  (if a b #f))

(and #t #t)  ; => #t
(and #t #f)  ; => #f
```

### OR

```scheme
(define (or a b)
  (if a #t b))

(or #t #f)   ; => #t
(or #f #f)   ; => #f
```

### NOT

```scheme
(define (not x)
  (if x #f #t))

(not #t)  ; => #f
(not #f)  ; => #t
```

### XOR

```scheme
(define (xor a b)
  (if a
    (not b)
    b))

(xor #t #f)  ; => #t
(xor #t #t)  ; => #f
```

## Number Properties

### Even/Odd

```scheme
(define (even? n)
  (eq? (mod n 2) 0))

(define (odd? n)
  (not (even? n)))

(even? 4)  ; => #t
(odd? 7)   ; => #t
```

### Absolute Value

```scheme
(define (abs x)
  (if (< x 0)
    (- 0 x)
    x))

(abs -42)  ; => 42
(abs 42)   ; => 42
```

### Sign Function

```scheme
(define (sign x)
  (if (< x 0)
    -1
    (if (eq? x 0)
      0
      1)))

(sign -5)  ; => -1
(sign 0)   ; => 0
(sign 5)   ; => 1
```

## Advanced Examples

### Prime Number Checker

```scheme
(define (is-divisible? n d)
  (eq? (mod n d) 0))

(define (check-divisors n d)
  (if (eq? d 1)
    #t
    (if (is-divisible? n d)
      #f
      (check-divisors n (- d 1)))))

(define (is-prime? n)
  (if (< n 2)
    #f
    (check-divisors n (- n 1))))

(is-prime? 2)   ; => #t
(is-prime? 17)  ; => #t
(is-prime? 18)  ; => #f
```

### Sum of Divisors

```scheme
(define (sum-divisors-helper n d acc)
  (if (eq? d 0)
    acc
    (if (eq? (mod n d) 0)
      (sum-divisors-helper n (- d 1) (+ acc d))
      (sum-divisors-helper n (- d 1) acc))))

(define (sum-divisors n)
  (sum-divisors-helper n (- n 1) 0))

(sum-divisors 12)  ; => 16 (1+2+3+4+6)
```

### Perfect Number

```scheme
(define (is-perfect? n)
  (eq? (sum-divisors n) n))

(is-perfect? 6)   ; => #t (1+2+3=6)
(is-perfect? 28)  ; => #t (1+2+4+7+14=28)
```

### Collatz Sequence Length

```scheme
(define (collatz-helper n count)
  (if (eq? n 1)
    count
    (if (even? n)
      (collatz-helper (div n 2) (+ count 1))
      (collatz-helper (+ (* n 3) 1) (+ count 1)))))

(define (collatz-length n)
  (collatz-helper n 0))

(collatz-length 10)  ; => 6
```

### Digit Sum

```scheme
(define (digit-sum-helper n acc)
  (if (eq? n 0)
    acc
    (digit-sum-helper 
      (div n 10) 
      (+ acc (mod n 10)))))

(define (digit-sum n)
  (digit-sum-helper n 0))

(digit-sum 123)   ; => 6
(digit-sum 9876)  ; => 30
```

## Higher-Order Functions

### Apply Twice

```scheme
(define (apply-twice f x)
  (f (f x)))

(define (double x) (* x 2))

(apply-twice double 3)  ; => 12
```

### Compose

```scheme
(define (compose f g x)
  (f (g x)))

(define (add1 x) (+ x 1))
(define (mult2 x) (* x 2))

(compose add1 mult2 5)  ; => 11  ((5*2)+1)
```

### Repeat

```scheme
(define (repeat f n x)
  (if (eq? n 0)
    x
    (repeat f (- n 1) (f x))))

(define (inc x) (+ x 1))

(repeat inc 5 0)  ; => 5
```

## Pattern Examples

### Counter

```scheme
(define counter 0)

(define (increment)
  (define counter (+ counter 1))
  counter)

; Note: This pattern requires mutable state
; which is not available in pure GLaDOS
```

### Accumulator Pattern

```scheme
(define (sum-to-n-helper n acc)
  (if (eq? n 0)
    acc
    (sum-to-n-helper (- n 1) (+ acc n))))

(define (sum-to-n n)
  (sum-to-n-helper n 0))

(sum-to-n 100)  ; => 5050
```

---

## Tips for Writing GLaDOS Code

1. **Use tail recursion** for functions that might recurse deeply
2. **Break complex functions** into smaller helper functions
3. **Name your functions clearly** - use `?` suffix for predicates
4. **Add comments** to explain non-obvious logic
5. **Test incrementally** - build up complex functions from simple parts

---

[← Back to Documentation](introduction.md)
