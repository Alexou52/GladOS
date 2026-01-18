{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- VMSpec.hs
--}

module VMSpec (spec) where

import Test.Hspec
import Bytecode
import VM

expectValue :: Program -> Value -> Expectation
expectValue prog val = fmap outputValue (runProgram prog) `shouldBe` Right val

spec :: Spec
spec = do
    describe "VM - Basic operations" $ do
        it "executes PUSH and HALT" $ do
            let prog = [PUSH (VInt 42), HALT]
            expectValue prog (VInt 42)
        
        it "executes addition" $ do
            let prog = [PUSH (VInt 2), PUSH (VInt 3), ADD, HALT]
            expectValue prog (VInt 5)
        
        it "executes subtraction" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 3), SUB, HALT]
            expectValue prog (VInt 7)
        
        it "executes multiplication" $ do
            let prog = [PUSH (VInt 6), PUSH (VInt 7), MUL, HALT]
            expectValue prog (VInt 42)
        
        it "executes division" $ do
            let prog = [PUSH (VInt 42), PUSH (VInt 6), DIV, HALT]
            expectValue prog (VInt 7)
        
        it "executes modulo" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 3), MOD, HALT]
            expectValue prog (VInt 1)

    describe "VM - Stack operations" $ do
        it "executes POP" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 2), POP, HALT]
            expectValue prog (VInt 1)
        
        it "executes DUP" $ do
            let prog = [PUSH (VInt 42), DUP, ADD, HALT]
            expectValue prog (VInt 84)
        
        it "executes SWAP" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 2), SWAP, SUB, HALT]
            expectValue prog (VInt 1)

    describe "VM - Variables" $ do
        it "stores and loads variable" $ do
            let prog = [PUSH (VInt 42), STORE "x", LOAD "x", HALT]
            expectValue prog (VInt 42)
        
        it "handles multiple variables" $ do
            let prog = [PUSH (VInt 10), STORE "a", PUSH (VInt 20), STORE "b", 
                       LOAD "a", LOAD "b", ADD, HALT]
            expectValue prog (VInt 30)

    describe "VM - Comparisons" $ do
        it "executes EQ (equal)" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 5), EQ_, HALT]
            expectValue prog (VBool True)
        
        it "executes EQ (not equal)" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 3), EQ_, HALT]
            expectValue prog (VBool False)
        
        it "executes LT" $ do
            let prog = [PUSH (VInt 3), PUSH (VInt 5), LT_, HALT]
            expectValue prog (VBool True)
        
        it "executes GT" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 3), GT_, HALT]
            expectValue prog (VBool True)

    describe "VM - Jumps" $ do
        it "executes unconditional JMP" $ do
            let prog = [PUSH (VInt 1), JMP 2, PUSH (VInt 99), POP, PUSH (VInt 42), HALT]
            expectValue prog (VInt 42)
        
        it "executes JMP_IF_FALSE (takes jump)" $ do
            let prog = [PUSH (VBool False), JMP_IF_FALSE 2, PUSH (VInt 1), HALT, PUSH (VInt 2), HALT]
            expectValue prog (VInt 2)
        
        it "executes JMP_IF_FALSE (no jump)" $ do
            let prog = [PUSH (VBool True), JMP_IF_FALSE 2, PUSH (VInt 1), HALT, PUSH (VInt 2), HALT]
            expectValue prog (VInt 1)

        it "treats empty list as false" $ do
            let prog = [MAKE_LIST 0, JMP_IF_FALSE 2, PUSH (VInt 1), HALT, PUSH (VInt 2), HALT]
            expectValue prog (VInt 2)

        it "treats non-empty list as true" $ do
            let prog = [PUSH (VInt 1), MAKE_LIST 1, JMP_IF_FALSE 2, PUSH (VInt 1), HALT, PUSH (VInt 2), HALT]
            expectValue prog (VInt 1)

    describe "VM - If expression pattern" $ do
        it "evaluates if true branch" $ do
            let prog = [PUSH (VBool True), JMP_IF_FALSE 2, PUSH (VInt 1), JMP 1, PUSH (VInt 2), HALT]
            expectValue prog (VInt 1)
        
        it "evaluates if false branch" $ do
            let prog = [PUSH (VBool False), JMP_IF_FALSE 2, PUSH (VInt 1), JMP 1, PUSH (VInt 2), HALT]
            expectValue prog (VInt 2)

    describe "VM - Closures" $ do
        it "creates and calls simple closure" $ do
            let body = [LOAD "x", PUSH (VInt 2), MUL, RET]
            let prog = [PUSH (VInt 21), MAKE_CLOSURE ["x"] body, CALL 1, HALT]
            expectValue prog (VInt 42)
        
        it "creates closure with multiple params" $ do
            let body = [LOAD "a", LOAD "b", ADD, RET]
            let prog = [PUSH (VInt 10), PUSH (VInt 32), MAKE_CLOSURE ["a", "b"] body, CALL 2, HALT]
            expectValue prog (VInt 42)

    describe "VM - Error handling" $ do
        it "reports division by zero" $ do
            let prog = [PUSH (VInt 42), PUSH (VInt 0), DIV, HALT]
            runProgram prog `shouldBe` Left "DIV: division by zero"
        
        it "reports undefined variable" $ do
            let prog = [LOAD "undefined", HALT]
            runProgram prog `shouldBe` Left "line 1: undefined variable 'undefined'"
        
        it "reports empty stack on POP" $ do
            let prog = [POP, HALT]
            runProgram prog `shouldBe` Left "POP: empty stack"

    describe "VM - Boolean operations" $ do
        it "executes NOT" $ do
            let prog = [PUSH (VBool True), NOT, HALT]
            expectValue prog (VBool False)
        
        it "executes AND" $ do
            let prog = [PUSH (VBool True), PUSH (VBool False), AND, HALT]
            expectValue prog (VBool False)
        
        it "executes OR" $ do
            let prog = [PUSH (VBool False), PUSH (VBool True), OR, HALT]
            expectValue prog (VBool True)

    describe "VM - Complex expressions" $ do
        it "evaluates (2 + 3) * 4" $ do
            let prog = [PUSH (VInt 2), PUSH (VInt 3), ADD, PUSH (VInt 4), MUL, HALT]
            expectValue prog (VInt 20)
        
        it "evaluates nested arithmetic" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 5), SUB, 
                       PUSH (VInt 2), MUL, HALT]
            expectValue prog (VInt 10)

    describe "VM - String operations" $ do
        it "pushes string value" $ do
            let prog = [PUSH (VString "hello"), HALT]
            expectValue prog (VString "hello")
        
        it "concatenates strings" $ do
            let prog = [PUSH (VString "hello"), PUSH (VString " world"), CONCAT, HALT]
            expectValue prog (VString "hello world")
        
        it "concatenates string with int" $ do
            let prog = [PUSH (VString "value: "), PUSH (VInt 42), CONCAT, HALT]
            expectValue prog (VString "value: 42")
        
        it "concatenates int with string" $ do
            let prog = [PUSH (VInt 42), PUSH (VString " items"), CONCAT, HALT]
            expectValue prog (VString "42 items")

    describe "VM - List operations" $ do
        it "creates empty list" $ do
            let prog = [MAKE_LIST 0, HALT]
            expectValue prog (VList [])
        
        it "creates list with elements" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 2), PUSH (VInt 3), MAKE_LIST 3, HALT]
            expectValue prog (VList [VInt 1, VInt 2, VInt 3])
        
        it "gets element from list" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 20), MAKE_LIST 2, PUSH (VInt 1), LIST_GET, HALT]
            expectValue prog (VInt 20)
        
        it "gets list length" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 2), MAKE_LIST 2, LIST_LEN, HALT]
            expectValue prog (VInt 2)
        
        it "reports list index out of bounds" $ do
            let prog = [PUSH (VInt 1), MAKE_LIST 1, PUSH (VInt 5), LIST_GET, HALT]
            runProgram prog `shouldBe` Left "LIST_GET: index 5 out of bounds"

        it "appends to list" $ do
            let prog = [PUSH (VInt 1), MAKE_LIST 1, PUSH (VInt 2), LOAD "append", CALL 2, HALT]
            expectValue prog (VList [VInt 1, VInt 2])

        it "sets list element" $ do
            let prog = [PUSH (VInt 1), MAKE_LIST 1, PUSH (VInt 0), PUSH (VInt 42), LOAD "set", CALL 3, HALT]
            expectValue prog (VList [VInt 42])

        it "concatenates lists" $ do
            let prog = [PUSH (VInt 1), MAKE_LIST 1, PUSH (VInt 2), MAKE_LIST 1, LOAD "concat", CALL 2, HALT]
            expectValue prog (VList [VInt 1, VInt 2])

    describe "VM - Nil value" $ do
        it "pushes nil" $ do
            let prog = [PUSH VNil, HALT]
            expectValue prog VNil

    describe "VM - NEQ operation" $ do
        it "executes NEQ (not equal)" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 3), NEQ, HALT]
            expectValue prog (VBool True)
        
        it "executes NEQ (equal values)" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 5), NEQ, HALT]
            expectValue prog (VBool False)
        
        it "executes NEQ on booleans" $ do
            let prog = [PUSH (VBool True), PUSH (VBool False), NEQ, HALT]
            expectValue prog (VBool True)

    describe "VM - LTE and GTE" $ do
        it "executes LTE (less)" $ do
            let prog = [PUSH (VInt 3), PUSH (VInt 5), LTE, HALT]
            expectValue prog (VBool True)
        
        it "executes LTE (equal)" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 5), LTE, HALT]
            expectValue prog (VBool True)
        
        it "executes GTE (greater)" $ do
            let prog = [PUSH (VInt 7), PUSH (VInt 5), GTE, HALT]
            expectValue prog (VBool True)
        
        it "executes GTE (equal)" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 5), GTE, HALT]
            expectValue prog (VBool True)

    describe "VM - NEG operation" $ do
        it "negates positive integer" $ do
            let prog = [PUSH (VInt 42), NEG, HALT]
            expectValue prog (VInt (-42))
        
        it "negates negative integer" $ do
            let prog = [PUSH (VInt (-10)), NEG, HALT]
            expectValue prog (VInt 10)

    describe "VM - JMP_IF_TRUE" $ do
        it "takes jump when true" $ do
            let prog = [PUSH (VBool True), JMP_IF_TRUE 2, PUSH (VInt 1), HALT, PUSH (VInt 2), HALT]
            expectValue prog (VInt 2)
        
        it "skips jump when false" $ do
            let prog = [PUSH (VBool False), JMP_IF_TRUE 2, PUSH (VInt 1), HALT, PUSH (VInt 2), HALT]
            expectValue prog (VInt 1)

    describe "VM - NOP and LINE" $ do
        it "executes NOP" $ do
            let prog = [NOP, PUSH (VInt 42), HALT]
            expectValue prog (VInt 42)
        
        it "executes LINE" $ do
            let prog = [LINE 10, PUSH (VInt 42), HALT]
            expectValue prog (VInt 42)

    describe "VM - Print" $ do
        it "prints single value" $ do
            let prog = [PUSH (VInt 42), PUSH (VInt 1), PRINT, HALT]
            case runProgram prog of
                Right out -> outputLines out `shouldBe` ["42"]
                Left err -> expectationFailure err
        
        it "prints multiple values" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 2), PUSH (VInt 2), PRINT, HALT]
            case runProgram prog of
                Right out -> outputLines out `shouldBe` ["1 2"]
                Left err -> expectationFailure err

    describe "VM - Error handling extended" $ do
        it "reports DUP on empty stack" $ do
            runProgram [DUP, HALT] `shouldBe` Left "DUP: empty stack"
        
        it "reports SWAP with insufficient stack" $ do
            runProgram [PUSH (VInt 1), SWAP, HALT] `shouldBe` Left "SWAP: insufficient stack"
        
        it "reports STORE on empty stack" $ do
            runProgram [STORE "x", HALT] `shouldBe` Left "STORE: empty stack"
        
        it "reports MOD by zero" $ do
            runProgram [PUSH (VInt 10), PUSH (VInt 0), MOD, HALT] `shouldBe` Left "MOD: division by zero"
        
        it "reports EQ on invalid operands" $ do
            runProgram [PUSH (VInt 1), PUSH (VString "x"), EQ_, HALT] `shouldBe` Left "EQ: invalid operands"
        
        it "reports NEQ on invalid operands" $ do
            runProgram [PUSH (VInt 1), PUSH (VString "x"), NEQ, HALT] `shouldBe` Left "NEQ: invalid operands"
        
        it "reports NOT on non-boolean" $ do
            runProgram [PUSH (VInt 1), NOT, HALT] `shouldBe` Left "NOT: expected boolean"
        
        it "reports AND on non-boolean" $ do
            runProgram [PUSH (VInt 1), PUSH (VInt 2), AND, HALT] `shouldBe` Left "AND: expected booleans"
        
        it "reports OR on non-boolean" $ do
            runProgram [PUSH (VInt 1), PUSH (VInt 2), OR, HALT] `shouldBe` Left "OR: expected booleans"
        
        it "reports NEG on non-integer" $ do
            runProgram [PUSH (VBool True), NEG, HALT] `shouldBe` Left "NEG: expected integer"
        
        it "reports JMP_IF_FALSE on non-boolean" $ do
            runProgram [PUSH (VInt 1), JMP_IF_FALSE 0, HALT] `shouldBe` Left "JMP_IF_*: expected boolean or list"
        
        it "reports JMP_IF_TRUE on non-boolean" $ do
            runProgram [PUSH (VInt 1), JMP_IF_TRUE 0, HALT] `shouldBe` Left "JMP_IF_*: expected boolean or list"
        
        it "reports DIV on invalid operands" $ do
            runProgram [PUSH (VString "a"), PUSH (VInt 2), DIV, HALT] `shouldBe` Left "DIV: invalid operands"
        
        it "reports MOD on invalid operands" $ do
            runProgram [PUSH (VString "a"), PUSH (VInt 2), MOD, HALT] `shouldBe` Left "MOD: invalid operands"
        
        it "reports CONCAT on invalid operands" $ do
            runProgram [PUSH (VInt 1), PUSH (VInt 2), CONCAT, HALT] `shouldBe` Left "CONCAT: invalid operands"
        
        it "reports LIST_GET on non-list" $ do
            runProgram [PUSH (VInt 1), PUSH (VInt 0), LIST_GET, HALT] `shouldBe` Left "LIST_GET: expected list and integer"
        
        it "reports LIST_LEN on non-list" $ do
            runProgram [PUSH (VInt 1), LIST_LEN, HALT] `shouldBe` Left "LIST_LEN: expected list"
        
        it "reports BREAK outside loop" $ do
            runProgram [BREAK, HALT] `shouldBe` Left "BREAK: break outside of loop"
        
        it "reports CONTINUE outside loop" $ do
            runProgram [CONTINUE, HALT] `shouldBe` Left "CONTINUE: continue outside of loop"

    describe "VM - CALL errors" $ do
        it "reports wrong arg count" $ do
            let body = [LOAD "x", RET]
            let prog = [PUSH (VInt 1), PUSH (VInt 2), MAKE_CLOSURE ["x"] body, CALL 2, HALT]
            runProgram prog `shouldBe` Left "CALL: expected 1 args, got 2"
        
        it "reports call on non-function" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 42), CALL 1, HALT]
            runProgram prog `shouldBe` Left "CALL: not a function"

    describe "VM - Builtins" $ do
        it "calls builtin +" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 20), LOAD "+", CALL 2, HALT]
            expectValue prog (VInt 30)
        
        it "calls builtin -" $ do
            let prog = [PUSH (VInt 30), PUSH (VInt 10), LOAD "-", CALL 2, HALT]
            expectValue prog (VInt 20)
        
        it "calls builtin *" $ do
            let prog = [PUSH (VInt 6), PUSH (VInt 7), LOAD "*", CALL 2, HALT]
            expectValue prog (VInt 42)
        
        it "calls builtin div" $ do
            let prog = [PUSH (VInt 42), PUSH (VInt 6), LOAD "div", CALL 2, HALT]
            expectValue prog (VInt 7)
        
        it "reports builtin div by zero" $ do
            let prog = [PUSH (VInt 42), PUSH (VInt 0), LOAD "div", CALL 2, HALT]
            runProgram prog `shouldBe` Left "div: division by zero"
        
        it "calls builtin mod" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 3), LOAD "mod", CALL 2, HALT]
            expectValue prog (VInt 1)
        
        it "reports builtin mod by zero" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 0), LOAD "mod", CALL 2, HALT]
            runProgram prog `shouldBe` Left "mod: division by zero"
        
        it "calls builtin eq?" $ do
            let prog = [PUSH (VInt 5), PUSH (VInt 5), LOAD "eq?", CALL 2, HALT]
            expectValue prog (VBool True)
        
        it "calls builtin <" $ do
            let prog = [PUSH (VInt 3), PUSH (VInt 5), LOAD "<", CALL 2, HALT]
            expectValue prog (VBool True)
        
        it "calls builtin >" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 5), LOAD ">", CALL 2, HALT]
            expectValue prog (VBool True)
        
        it "calls builtin len on list" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 2), MAKE_LIST 2, LOAD "len", CALL 1, HALT]
            expectValue prog (VInt 2)
        
        it "calls builtin len on string" $ do
            let prog = [PUSH (VString "hello"), LOAD "len", CALL 1, HALT]
            expectValue prog (VInt 5)
        
        it "calls builtin get on list" $ do
            let prog = [PUSH (VInt 10), PUSH (VInt 20), MAKE_LIST 2, PUSH (VInt 0), LOAD "get", CALL 2, HALT]
            expectValue prog (VInt 10)
        
        it "calls builtin get on string" $ do
            let prog = [PUSH (VString "hello"), PUSH (VInt 1), LOAD "get", CALL 2, HALT]
            expectValue prog (VString "e")
        
        it "reports builtin get out of bounds on list" $ do
            let prog = [PUSH (VInt 1), MAKE_LIST 1, PUSH (VInt 5), LOAD "get", CALL 2, HALT]
            runProgram prog `shouldBe` Left "get: index 5 out of bounds"
        
        it "reports builtin get out of bounds on string" $ do
            let prog = [PUSH (VString "hi"), PUSH (VInt 10), LOAD "get", CALL 2, HALT]
            runProgram prog `shouldBe` Left "get: index 10 out of bounds"
        
        it "reports unknown builtin" $ do
            let prog = [LOAD "unknown_builtin", CALL 0, HALT]
            case runProgram prog of
                Left err -> err `shouldContain` "undefined"
                Right _ -> expectationFailure "Should have failed"

    describe "VM - Argv and Argc" $ do
        it "has empty argv by default" $ do
            let prog = [LOAD "argv", HALT]
            expectValue prog (VList [])
        
        it "has zero argc by default" $ do
            let prog = [LOAD "argc", HALT]
            expectValue prog (VInt 0)
        
        it "has argv with args" $ do
            let prog = [LOAD "argv", HALT]
            case runProgramWithArgs ["a", "b"] prog of
                Right out -> outputValue out `shouldBe` VList [VString "a", VString "b"]
                Left err -> expectationFailure err
        
        it "has correct argc with args" $ do
            let prog = [LOAD "argc", HALT]
            case runProgramWithArgs ["x", "y", "z"] prog of
                Right out -> outputValue out `shouldBe` VInt 3
                Left err -> expectationFailure err

    describe "VM - RET edge cases" $ do
        it "halts on RET with empty call stack" $ do
            let prog = [PUSH (VInt 42), RET]
            expectValue prog (VInt 42)

    describe "VM - EQ on booleans" $ do
        it "compares equal booleans" $ do
            let prog = [PUSH (VBool True), PUSH (VBool True), EQ_, HALT]
            expectValue prog (VBool True)
        
        it "compares different booleans" $ do
            let prog = [PUSH (VBool True), PUSH (VBool False), EQ_, HALT]
            expectValue prog (VBool False)
