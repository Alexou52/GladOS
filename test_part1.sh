#!/bin/bash

echo "=== Testing GLaDOS Part 1 ==="
echo ""

echo "1. Test builtins (should output 11):"
./glados < examples/builtins1.scm
echo ""

echo "2. Test eq? (should output #t):"
./glados < examples/builtins2.scm
echo ""

echo "3. Test mod (should output #f):"
./glados < examples/builtins3.scm
echo ""

echo "4. Test define + multiply (should output 84):"
./glados < examples/foo.scm
echo ""

echo "5. Test function syntax (should output 7):"
./glados < examples/function1.scm
echo ""

echo "6. Test user-defined > (should output #t):"
./glados < examples/superior.scm
echo ""

echo "7. Test factorial recursion (should output 3628800):"
./glados < examples/factorial.scm
echo ""

echo "8. Test error - unbound variable (should show error and exit 84):"
./glados < examples/error.scm
echo "Exit code: $?"
echo ""

echo "9. Test error - division by zero (should show error and exit 84):"
./glados < examples/divzero.scm
echo "Exit code: $?"
echo ""

echo "=== All Part 1 requirements tested! ==="
