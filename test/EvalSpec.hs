-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- EvalSpec.hs - Tests for Eval module
--
module EvalSpec (spec) where

import Test.Hspec
import AST
import Eval

-- Helper to extract result from (Maybe Ast, Env) tuple
evalResult :: Ast -> Maybe Ast
evalResult ast = fst (evalAST ast)

spec :: Spec
spec = do
    describe "evalResult - Basic values" $ do
        it "evaluates integer to itself" $ do
            evalResult (AstInt 42) `shouldBe` Just (AstInt 42)
        
        it "evaluates boolean to itself" $ do
            evalResult (AstBool True) `shouldBe` Just (AstBool True)
    
    describe "evalResult - Arithmetic operations" $ do
        it "evaluates simple addition" $ do
            let expr = AstCall (AstSymbol "+") [AstInt 2, AstInt 3]
            evalResult expr `shouldBe` Just (AstInt 5)
        
        it "evaluates simple subtraction" $ do
            let expr = AstCall (AstSymbol "-") [AstInt 10, AstInt 3]
            evalResult expr `shouldBe` Just (AstInt 7)
        
        it "evaluates simple multiplication" $ do
            let expr = AstCall (AstSymbol "*") [AstInt 6, AstInt 7]
            evalResult expr `shouldBe` Just (AstInt 42)
        
        it "evaluates simple division" $ do
            let expr = AstCall (AstSymbol "div") [AstInt 42, AstInt 6]
            evalResult expr `shouldBe` Just (AstInt 7)
        
        it "handles division by zero" $ do
            let expr = AstCall (AstSymbol "div") [AstInt 42, AstInt 0]
            evalResult expr `shouldBe` Nothing
        
        it "evaluates multiple additions" $ do
            let expr = AstCall (AstSymbol "+") [AstInt 1, AstInt 2, AstInt 3, AstInt 4]
            evalResult expr `shouldBe` Just (AstInt 10)
    
    describe "evalResult - Nested expressions" $ do
        it "evaluates (* (+ 4 3) 6)" $ do
            let expr = AstCall (AstSymbol "*") 
                            [AstCall (AstSymbol "+") [AstInt 4, AstInt 3], AstInt 6]
            evalResult expr `shouldBe` Just (AstInt 42)
        
        it "evaluates (+ 2 (* 3 4))" $ do
            let expr = AstCall (AstSymbol "+") 
                            [AstInt 2, AstCall (AstSymbol "*") [AstInt 3, AstInt 4]]
            evalResult expr `shouldBe` Just (AstInt 14)
        
        it "evaluates (div (* 6 7) 3)" $ do
            let expr = AstCall (AstSymbol "div") 
                            [AstCall (AstSymbol "*") [AstInt 6, AstInt 7], AstInt 3]
            evalResult expr `shouldBe` Just (AstInt 14)
    
    describe "evalResult - Comparison operations" $ do
        it "evaluates < correctly (true)" $ do
            let expr = AstCall (AstSymbol "<") [AstInt 3, AstInt 5]
            evalResult expr `shouldBe` Just (AstBool True)
        
        it "evaluates < correctly (false)" $ do
            let expr = AstCall (AstSymbol "<") [AstInt 5, AstInt 3]
            evalResult expr `shouldBe` Just (AstBool False)
        
        it "evaluates eq? correctly" $ do
            let expr = AstCall (AstSymbol "eq?") [AstInt 5, AstInt 5]
            evalResult expr `shouldBe` Just (AstBool True)
        
        it "evaluates eq? correctly (false)" $ do
            let expr = AstCall (AstSymbol "eq?") [AstInt 5, AstInt 3]
            evalResult expr `shouldBe` Just (AstBool False)
    
    describe "evalResult - If expressions" $ do
        it "evaluates if with true condition" $ do
            let expr = If (AstBool True) (AstInt 10) (AstInt 20)
            evalResult expr `shouldBe` Just (AstInt 10)
        
        it "evaluates if with false condition" $ do
            let expr = If (AstBool False) (AstInt 10) (AstInt 20)
            evalResult expr `shouldBe` Just (AstInt 20)
        
        it "evaluates if with comparison" $ do
            let expr = If (AstCall (AstSymbol "<") [AstInt 5, AstInt 10])
                          (AstCall (AstSymbol "*") [AstInt 2, AstInt 3])
                          (AstCall (AstSymbol "+") [AstInt 1, AstInt 1])
            evalResult expr `shouldBe` Just (AstInt 6)
    
    describe "evalResult - Boolean operations" $ do
        it "evaluates eq? for booleans" $ do
            let expr = AstCall (AstSymbol "eq?") [AstBool True, AstBool True]
            evalResult expr `shouldBe` Just (AstBool True)
    
    describe "evalResult - Modulo operation" $ do
        it "evaluates mod correctly" $ do
            let expr = AstCall (AstSymbol "mod") [AstInt 10, AstInt 3]
            evalResult expr `shouldBe` Just (AstInt 1)
        
        it "handles mod by zero" $ do
            let expr = AstCall (AstSymbol "mod") [AstInt 10, AstInt 0]
            evalResult expr `shouldBe` Nothing
    
    describe "evalResult - Subtraction variations" $ do
        it "evaluates subtraction of multiple values" $ do
            let expr = AstCall (AstSymbol "-") [AstInt 10, AstInt 3, AstInt 2]
            evalResult expr `shouldBe` Just (AstInt 5)
        
        it "evaluates negative result" $ do
            let expr = AstCall (AstSymbol "-") [AstInt 3, AstInt 10]
            evalResult expr `shouldBe` Just (AstInt (-7))
    
    describe "evalResult - Define behavior" $ do
        it "evaluates define and returns the value" $ do
            let expr = Define "x" (AstInt 42)
            evalResult expr `shouldBe` Just (AstInt 42)
        
        it "evaluates define with nested expression" $ do
            let expr = Define "foo" (AstCall (AstSymbol "+") [AstInt 2, AstInt 3])
            evalResult expr `shouldBe` Just (AstInt 5)
    
    describe "evalResult - Lambda evaluation" $ do
        it "evaluates lambda to itself" $ do
            let expr = Lambda ["x"] (AstSymbol "x")
            evalResult expr `shouldBe` Just expr
    
    describe "evalResult - If with non-boolean condition" $ do
        it "returns Nothing when condition is not boolean" $ do
            let expr = If (AstInt 1) (AstInt 10) (AstInt 20)
            evalResult expr `shouldBe` Nothing
    
    describe "evalResult - Error cases" $ do
        it "returns Nothing for unknown function" $ do
            let expr = AstCall (AstSymbol "unknown") [AstInt 1]
            evalResult expr `shouldBe` Nothing
        
        it "returns Nothing for invalid arguments" $ do
            let expr = AstCall (AstSymbol "+") [AstBool True, AstInt 1]
            evalResult expr `shouldBe` Nothing
        
        it "returns Nothing for arithmetic with insufficient args" $ do
            let expr = AstCall (AstSymbol "+") [AstInt 1]
            evalResult expr `shouldBe` Nothing
        
        it "returns Nothing for comparison with wrong arg count" $ do
            let expr = AstCall (AstSymbol "<") [AstInt 1]
            evalResult expr `shouldBe` Nothing
        
        it "returns Nothing for symbol lookup in empty env" $ do
            evalResult (AstSymbol "undefined") `shouldBe` Nothing
    
    describe "evalResult - Complex nested scenarios" $ do
        it "evaluates nested if with arithmetic" $ do
            let expr = If (AstCall (AstSymbol "<") [AstInt 2, AstInt 5])
                          (AstCall (AstSymbol "*") [AstInt 10, AstInt 10])
                          (AstInt 0)
            evalResult expr `shouldBe` Just (AstInt 100)
        
        it "evaluates deeply nested arithmetic" $ do
            let expr = AstCall (AstSymbol "+")
                            [ AstCall (AstSymbol "*") [AstInt 2, AstInt 3]
                            , AstCall (AstSymbol "div") [AstInt 8, AstInt 2]
                            , AstCall (AstSymbol "-") [AstInt 10, AstInt 5]
                            ]
            evalResult expr `shouldBe` Just (AstInt 15)

    describe "evalResult - Greater than comparison" $ do
        it "evaluates > correctly (true)" $ do
            let expr = AstCall (AstSymbol ">") [AstInt 10, AstInt 5]
            evalResult expr `shouldBe` Just (AstBool True)
        
        it "evaluates > correctly (false)" $ do
            let expr = AstCall (AstSymbol ">") [AstInt 3, AstInt 5]
            evalResult expr `shouldBe` Just (AstBool False)
        
        it "evaluates > correctly (equal values)" $ do
            let expr = AstCall (AstSymbol ">") [AstInt 5, AstInt 5]
            evalResult expr `shouldBe` Just (AstBool False)

    describe "evalResult - Lambda application" $ do
        it "applies simple lambda" $ do
            let lambda = Lambda ["x"] (AstCall (AstSymbol "*") [AstSymbol "x", AstInt 2])
            let call = AstCall lambda [AstInt 21]
            evalResult call `shouldBe` Just (AstInt 42)
        
        it "applies lambda with multiple params" $ do
            let lambda = Lambda ["a", "b"] (AstCall (AstSymbol "+") [AstSymbol "a", AstSymbol "b"])
            let call = AstCall lambda [AstInt 10, AstInt 32]
            evalResult call `shouldBe` Just (AstInt 42)
        
        it "returns Nothing for lambda with wrong arg count" $ do
            let lambda = Lambda ["x", "y"] (AstSymbol "x")
            let call = AstCall lambda [AstInt 1]
            evalResult call `shouldBe` Nothing

    describe "evalResult - Multiplication variations" $ do
        it "evaluates multiplication of multiple values" $ do
            let expr = AstCall (AstSymbol "*") [AstInt 2, AstInt 3, AstInt 4]
            evalResult expr `shouldBe` Just (AstInt 24)
        
        it "evaluates multiplication with zeros" $ do
            let expr = AstCall (AstSymbol "*") [AstInt 5, AstInt 0]
            evalResult expr `shouldBe` Just (AstInt 0)

    describe "evalResult - Division edge cases" $ do
        it "evaluates integer division correctly" $ do
            let expr = AstCall (AstSymbol "div") [AstInt 7, AstInt 2]
            evalResult expr `shouldBe` Just (AstInt 3)
        
        it "evaluates division with negative" $ do
            let expr = AstCall (AstSymbol "div") [AstInt (-10), AstInt 3]
            evalResult expr `shouldBe` Just (AstInt (-4))

    describe "evalResult - Modulo edge cases" $ do
        it "evaluates mod with larger divisor" $ do
            let expr = AstCall (AstSymbol "mod") [AstInt 3, AstInt 10]
            evalResult expr `shouldBe` Just (AstInt 3)
        
        it "evaluates mod with exact division" $ do
            let expr = AstCall (AstSymbol "mod") [AstInt 10, AstInt 5]
            evalResult expr `shouldBe` Just (AstInt 0)

    describe "evalResult - eq? edge cases" $ do
        it "evaluates eq? for same booleans (false)" $ do
            let expr = AstCall (AstSymbol "eq?") [AstBool False, AstBool False]
            evalResult expr `shouldBe` Just (AstBool True)
        
        it "evaluates eq? for different booleans" $ do
            let expr = AstCall (AstSymbol "eq?") [AstBool True, AstBool False]
            evalResult expr `shouldBe` Just (AstBool False)

    describe "evalResult - Define with complex expression" $ do
        it "evaluates define with if expression" $ do
            let expr = Define "x" (If (AstBool True) (AstInt 10) (AstInt 20))
            evalResult expr `shouldBe` Just (AstInt 10)

    describe "evalResult - Nested conditionals" $ do
        it "evaluates nested if expressions" $ do
            let inner = If (AstBool False) (AstInt 1) (AstInt 2)
            let outer = If (AstBool True) inner (AstInt 3)
            evalResult outer `shouldBe` Just (AstInt 2)

    describe "evalResult - Error propagation" $ do
        it "propagates error from nested expression" $ do
            let inner = AstCall (AstSymbol "div") [AstInt 1, AstInt 0]
            let outer = AstCall (AstSymbol "+") [inner, AstInt 1]
            evalResult outer `shouldBe` Nothing
        
        it "propagates error from condition" $ do
            let cond = AstCall (AstSymbol "unknown") []
            let expr = If cond (AstInt 1) (AstInt 2)
            evalResult expr `shouldBe` Nothing
