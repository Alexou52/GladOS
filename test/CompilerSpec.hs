{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- CompilerSpec.hs
--}

module CompilerSpec (spec) where

import Test.Hspec
import AST
import Bytecode
import Compiler

spec :: Spec
spec = do
    describe "Compiler - Literals" $ do
        it "compiles integer" $ do
            compileExpr (AstInt 42) `shouldBe` [PUSH (VInt 42)]
        
        it "compiles negative integer" $ do
            compileExpr (AstInt (-5)) `shouldBe` [PUSH (VInt (-5))]
        
        it "compiles boolean true" $ do
            compileExpr (AstBool True) `shouldBe` [PUSH (VBool True)]
        
        it "compiles boolean false" $ do
            compileExpr (AstBool False) `shouldBe` [PUSH (VBool False)]
        
        it "compiles symbol/variable" $ do
            compileExpr (AstSymbol "x") `shouldBe` [LOAD "x"]

    describe "Compiler - Arithmetic" $ do
        it "compiles addition" $ do
            let expr = AstCall (AstSymbol "+") [AstInt 2, AstInt 3]
            compileExpr expr `shouldBe` [PUSH (VInt 2), PUSH (VInt 3), ADD]
        
        it "compiles subtraction" $ do
            let expr = AstCall (AstSymbol "-") [AstInt 10, AstInt 3]
            compileExpr expr `shouldBe` [PUSH (VInt 10), PUSH (VInt 3), SUB]
        
        it "compiles multiplication" $ do
            let expr = AstCall (AstSymbol "*") [AstInt 6, AstInt 7]
            compileExpr expr `shouldBe` [PUSH (VInt 6), PUSH (VInt 7), MUL]
        
        it "compiles division" $ do
            let expr = AstCall (AstSymbol "div") [AstInt 42, AstInt 6]
            compileExpr expr `shouldBe` [PUSH (VInt 42), PUSH (VInt 6), DIV]
        
        it "compiles modulo" $ do
            let expr = AstCall (AstSymbol "mod") [AstInt 10, AstInt 3]
            compileExpr expr `shouldBe` [PUSH (VInt 10), PUSH (VInt 3), MOD]

    describe "Compiler - Comparisons" $ do
        it "compiles equality" $ do
            let expr = AstCall (AstSymbol "eq?") [AstInt 5, AstInt 5]
            compileExpr expr `shouldBe` [PUSH (VInt 5), PUSH (VInt 5), EQ_]
        
        it "compiles less than" $ do
            let expr = AstCall (AstSymbol "<") [AstInt 3, AstInt 5]
            compileExpr expr `shouldBe` [PUSH (VInt 3), PUSH (VInt 5), LT_]
        
        it "compiles greater than" $ do
            let expr = AstCall (AstSymbol ">") [AstInt 5, AstInt 3]
            compileExpr expr `shouldBe` [PUSH (VInt 5), PUSH (VInt 3), GT_]

    describe "Compiler - If expressions" $ do
        it "compiles simple if" $ do
            let expr = If (AstBool True) (AstInt 1) (AstInt 2)
            let code = compileExpr expr
            code `shouldBe` 
                [ PUSH (VBool True)
                , JMP_IF_FALSE 2
                , PUSH (VInt 1)
                , JMP 1
                , PUSH (VInt 2)
                ]
        
        it "compiles if with comparison" $ do
            let cond = AstCall (AstSymbol "<") [AstSymbol "x", AstInt 10]
            let expr = If cond (AstSymbol "x") (AstInt 0)
            let code = compileExpr expr
            length code `shouldBe` 7

    describe "Compiler - Lambda" $ do
        it "compiles simple lambda" $ do
            let expr = Lambda ["x"] (AstCall (AstSymbol "*") [AstSymbol "x", AstInt 2])
            let code = compileExpr expr
            case code of
                [MAKE_CLOSURE ["x"] body] -> do
                    body `shouldBe` [LOAD "x", PUSH (VInt 2), MUL, RET]
                _ -> expectationFailure $ "Unexpected: " ++ show code
        
        it "compiles lambda with multiple params" $ do
            let expr = Lambda ["a", "b"] (AstCall (AstSymbol "+") [AstSymbol "a", AstSymbol "b"])
            let code = compileExpr expr
            case code of
                [MAKE_CLOSURE ["a", "b"] body] -> do
                    body `shouldBe` [LOAD "a", LOAD "b", ADD, RET]
                _ -> expectationFailure $ "Unexpected: " ++ show code

    describe "Compiler - Definitions" $ do
        it "compiles variable definition" $ do
            let stmt = Define "x" (AstInt 42)
            compileStatement stmt `shouldBe` [PUSH (VInt 42), STORE "x"]
        
        it "compiles function definition" $ do
            let body = Lambda ["n"] (AstCall (AstSymbol "*") [AstSymbol "n", AstInt 2])
            let stmt = Define "double" body
            let code = compileStatement stmt
            case code of
                [MAKE_CLOSURE ["n"] _, STORE "double"] -> return ()
                _ -> expectationFailure $ "Unexpected: " ++ show code

    describe "Compiler - Function calls" $ do
        it "compiles user function call" $ do
            let expr = AstCall (AstSymbol "add") [AstInt 1, AstInt 2]
            compileExpr expr `shouldBe` 
                [PUSH (VInt 1), PUSH (VInt 2), LOAD "add", CALL 2]
        
        it "compiles nested call" $ do
            let inner = AstCall (AstSymbol "*") [AstInt 2, AstInt 3]
            let expr = AstCall (AstSymbol "foo") [inner, AstInt 4]
            let code = compileExpr expr
            code `shouldBe`
                [PUSH (VInt 2), PUSH (VInt 3), MUL, PUSH (VInt 4), LOAD "foo", CALL 2]

        it "compiles append(var, x) as mutation and returns nil" $ do
            let expr = AstCall (AstSymbol "append") [AstSymbol "xs", AstInt 2]
            let code = compileExpr expr
            code `shouldSatisfy` (elem (STORE "xs"))
            last code `shouldBe` PUSH VNil

        it "compiles set(var, i, x) as mutation and returns nil" $ do
            let expr = AstCall (AstSymbol "set") [AstSymbol "xs", AstInt 0, AstInt 42]
            let code = compileExpr expr
            code `shouldSatisfy` (elem (STORE "xs"))
            last code `shouldBe` PUSH VNil

    describe "Compiler - Full program" $ do
        it "compiles program with HALT" $ do
            let prog = [Define "x" (AstInt 42)]
            let code = compile prog
            last code `shouldBe` HALT
        
        it "compiles multiple statements" $ do
            let prog = [Define "x" (AstInt 1), Define "y" (AstInt 2)]
            let code = compile prog
            code `shouldBe` 
                [PUSH (VInt 1), STORE "x", PUSH (VInt 2), STORE "y", HALT]

    describe "Compiler - Strings" $ do
        it "compiles string literal" $ do
            compileExpr (AstString "hello") `shouldBe` [PUSH (VString "hello")]
        
        it "compiles empty string" $ do
            compileExpr (AstString "") `shouldBe` [PUSH (VString "")]

    describe "Compiler - Lists" $ do
        it "compiles empty list" $ do
            compileExpr (AstList []) `shouldBe` [MAKE_LIST 0]
        
        it "compiles list with integers" $ do
            compileExpr (AstList [AstInt 1, AstInt 2, AstInt 3]) `shouldBe`
                [PUSH (VInt 1), PUSH (VInt 2), PUSH (VInt 3), MAKE_LIST 3]
        
        it "compiles nested list" $ do
            let inner = AstList [AstInt 1]
            let expr = AstList [inner, AstInt 2]
            compileExpr expr `shouldBe`
                [PUSH (VInt 1), MAKE_LIST 1, PUSH (VInt 2), MAKE_LIST 2]

    describe "Compiler - For loops" $ do
        it "compiles simple for loop" $ do
            let forLoop = For "i" (AstInt 0) (AstInt 3) (Block [Print [AstSymbol "i"]])
            let code = compileExpr forLoop
            length code `shouldSatisfy` (> 5)
        
        it "compiles for loop as statement" $ do
            let forLoop = For "i" (AstInt 0) (AstInt 10) (Block [])
            let code = compileStatement forLoop
            length code `shouldSatisfy` (> 5)

        it "compiles foreach loop over list" $ do
            let loop = ForEach "x" (AstList [AstInt 1, AstInt 2]) (Block [Print [AstSymbol "x"]])
            let code = compileExpr loop
            code `shouldSatisfy` (elem (LOAD "len"))
            code `shouldSatisfy` (elem (LOAD "get"))

    describe "Compiler - While loops" $ do
        it "compiles simple while loop" $ do
            let whileLoop = While (AstBool True) (Block [AstInt 42])
            let code = compileExpr whileLoop
            length code `shouldSatisfy` (> 3)
        
        it "compiles while loop as statement" $ do
            let whileLoop = While (AstBool False) (Block [])
            let code = compileStatement whileLoop
            length code `shouldSatisfy` (> 3)

    describe "Compiler - Break and Continue" $ do
        it "compiles break" $ do
            compileExpr Break `shouldBe` [BREAK]
        
        it "compiles continue" $ do
            compileExpr Continue `shouldBe` [CONTINUE]
        
        it "compiles break as statement" $ do
            compileStatement Break `shouldBe` [BREAK]
        
        it "compiles continue as statement" $ do
            compileStatement Continue `shouldBe` [CONTINUE]

    describe "Compiler - Return" $ do
        it "compiles return expression" $ do
            compileExpr (Return (AstInt 42)) `shouldBe` [PUSH (VInt 42), RET]
        
        it "compiles return statement" $ do
            compileStatement (Return (AstSymbol "x")) `shouldBe` [LOAD "x", RET]

    describe "Compiler - Print" $ do
        it "compiles print statement" $ do
            let code = compileStatement (Print [AstInt 42])
            code `shouldBe` [PUSH (VInt 42), PUSH (VInt 1), PRINT, POP]
        
        it "compiles print expression" $ do
            let code = compileExpr (Print [AstInt 1, AstInt 2])
            code `shouldBe` [PUSH (VInt 1), PUSH (VInt 2), PUSH (VInt 2), PRINT]
        
        it "compiles print with multiple args" $ do
            let code = compileStatement (Print [AstSymbol "x", AstString "hello"])
            code `shouldBe` [LOAD "x", PUSH (VString "hello"), PUSH (VInt 2), PRINT, POP]

    describe "Compiler - Blocks" $ do
        it "compiles empty block" $ do
            compileExpr (Block []) `shouldBe` [PUSH VNil]
        
        it "compiles block with single expression" $ do
            let code = compileExpr (Block [AstInt 42])
            code `shouldBe` [PUSH (VInt 42)]
        
        it "compiles block with multiple statements" $ do
            let code = compileExpr (Block [Define "x" (AstInt 1), AstSymbol "x"])
            code `shouldBe` [PUSH (VInt 1), STORE "x", LOAD "x"]

    describe "Compiler - Located" $ do
        it "compiles located expression" $ do
            let loc = Located (SourcePos 5 1) (AstInt 42)
            let code = compileExpr loc
            code `shouldBe` [LINE 5, PUSH (VInt 42)]
        
        it "compiles located statement" $ do
            let loc = Located (SourcePos 10 1) (Define "x" (AstInt 1))
            let code = compileStatement loc
            code `shouldBe` [LINE 10, PUSH (VInt 1), STORE "x"]

    describe "Compiler - Comparison operators" $ do
        it "compiles not equal" $ do
            let expr = AstCall (AstSymbol "neq?") [AstInt 5, AstInt 3]
            compileExpr expr `shouldBe` [PUSH (VInt 5), PUSH (VInt 3), NEQ]
        
        it "compiles less than or equal" $ do
            let expr = AstCall (AstSymbol "<=") [AstInt 5, AstInt 5]
            compileExpr expr `shouldBe` [PUSH (VInt 5), PUSH (VInt 5), LTE]
        
        it "compiles greater than or equal" $ do
            let expr = AstCall (AstSymbol ">=") [AstInt 10, AstInt 5]
            compileExpr expr `shouldBe` [PUSH (VInt 10), PUSH (VInt 5), GTE]
        
        it "compiles not operator" $ do
            let expr = AstCall (AstSymbol "not") [AstBool True]
            compileExpr expr `shouldBe` [PUSH (VBool True), NOT]

    describe "Compiler - Unary minus" $ do
        it "compiles negation" $ do
            let expr = AstCall (AstSymbol "-") [AstInt 5]
            compileExpr expr `shouldBe` [PUSH (VInt 5), NEG]

    describe "Compiler - Invalid operations" $ do
        it "handles binary op with wrong args" $ do
            let expr = AstCall (AstSymbol "+") [AstInt 1]
            compileExpr expr `shouldBe` [PUSH VNil]
        
        it "handles unary op with wrong args" $ do
            let expr = AstCall (AstSymbol "not") []
            compileExpr expr `shouldBe` [PUSH VNil]

    describe "Compiler - Empty program" $ do
        it "compiles empty program" $ do
            compile [] `shouldBe` [PUSH VNil, HALT]

    describe "Compiler - Lambda body types" $ do
        it "compiles lambda with block body" $ do
            let body = Block [Return (AstInt 42)]
            let expr = Lambda ["x"] body
            let code = compileExpr expr
            case code of
                [MAKE_CLOSURE ["x"] bodyCode] ->
                    bodyCode `shouldBe` [PUSH (VInt 42), RET]
                _ -> expectationFailure $ "Unexpected: " ++ show code

    describe "Compiler - Define in expression context" $ do
        it "compiles define as expression" $ do
            let expr = Define "x" (AstInt 42)
            let code = compileExpr expr
            code `shouldBe` [PUSH (VInt 42), STORE "x", PUSH VNil]

    describe "Compiler - compileProgram" $ do
        it "compiles program without HALT" $ do
            let prog = [Define "x" (AstInt 42), Define "y" (AstInt 10)]
            let code = compileProgram prog
            code `shouldBe` [PUSH (VInt 42), STORE "x", PUSH (VInt 10), STORE "y"]
