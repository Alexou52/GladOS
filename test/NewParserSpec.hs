{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- NewParserSpec.hs
--}

module NewParserSpec (spec) where

import Test.Hspec
import AST
import NewParser

-- Helper to strip Located wrappers for easier testing
stripLocated :: Ast -> Ast
stripLocated (Located _ ast) = stripLocated ast
stripLocated (Define n v) = Define n (stripLocated v)
stripLocated (Lambda ps b) = Lambda ps (stripLocated b)
stripLocated (If c t e) = If (stripLocated c) (stripLocated t) (stripLocated e)
stripLocated (AstCall f args) = AstCall (stripLocated f) (map stripLocated args)
stripLocated (AstList xs) = AstList (map stripLocated xs)
stripLocated (Block stmts) = Block (map stripLocated stmts)
stripLocated (For v s e b) = For v (stripLocated s) (stripLocated e) (stripLocated b)
stripLocated (ForEach v it b) = ForEach v (stripLocated it) (stripLocated b)
stripLocated (While c b) = While (stripLocated c) (stripLocated b)
stripLocated (Return v) = Return (stripLocated v)
stripLocated (Print args) = Print (map stripLocated args)
stripLocated other = other

stripAll :: [Ast] -> [Ast]
stripAll = map stripLocated

spec :: Spec
spec = do
    describe "NewParser - Literals" $ do
        it "parses integers" $ do
            fmap stripAll (parseProgram "42") `shouldBe` Right [AstInt 42]
        
        it "parses negative integers" $ do
            fmap stripAll (parseProgram "-5") `shouldBe` Right [AstInt (-5)]
        
        it "parses booleans" $ do
            fmap stripAll (parseProgram "true") `shouldBe` Right [AstBool True]
            fmap stripAll (parseProgram "false") `shouldBe` Right [AstBool False]
        
        it "parses identifiers" $ do
            fmap stripAll (parseProgram "foo") `shouldBe` Right [AstSymbol "foo"]

    describe "NewParser - Definitions" $ do
        it "parses variable definition" $ do
            fmap stripAll (parseProgram "let x = 42") `shouldBe` 
                Right [Define "x" (AstInt 42)]
        
        it "parses function definition" $ do
            case fmap stripAll (parseProgram "def add(a, b): a + b") of
                Right [Define "add" (Lambda ["a", "b"] body)] -> 
                    body `shouldBe` AstCall (AstSymbol "+") [AstSymbol "a", AstSymbol "b"]
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Arithmetic" $ do
        it "parses addition" $ do
            parseExpression "2 + 3" `shouldBe` 
                Right (AstCall (AstSymbol "+") [AstInt 2, AstInt 3])
        
        it "parses subtraction" $ do
            parseExpression "10 - 3" `shouldBe` 
                Right (AstCall (AstSymbol "-") [AstInt 10, AstInt 3])
        
        it "parses multiplication" $ do
            parseExpression "6 * 7" `shouldBe` 
                Right (AstCall (AstSymbol "*") [AstInt 6, AstInt 7])
        
        it "parses division" $ do
            parseExpression "42 / 6" `shouldBe` 
                Right (AstCall (AstSymbol "div") [AstInt 42, AstInt 6])
        
        it "parses modulo" $ do
            parseExpression "10 % 3" `shouldBe` 
                Right (AstCall (AstSymbol "mod") [AstInt 10, AstInt 3])

    describe "NewParser - Operator precedence" $ do
        it "respects multiplication over addition" $ do
            parseExpression "2 + 3 * 4" `shouldBe` 
                Right (AstCall (AstSymbol "+") 
                    [AstInt 2, 
                     AstCall (AstSymbol "*") [AstInt 3, AstInt 4]])
        
        it "respects parentheses" $ do
            parseExpression "(2 + 3) * 4" `shouldBe` 
                Right (AstCall (AstSymbol "*") 
                    [AstCall (AstSymbol "+") [AstInt 2, AstInt 3], 
                     AstInt 4])

    describe "NewParser - Comparisons" $ do
        it "parses equality" $ do
            parseExpression "5 == 5" `shouldBe` 
                Right (AstCall (AstSymbol "eq?") [AstInt 5, AstInt 5])
        
        it "parses less than" $ do
            parseExpression "3 < 5" `shouldBe` 
                Right (AstCall (AstSymbol "<") [AstInt 3, AstInt 5])
        
        it "parses greater than" $ do
            parseExpression "5 > 3" `shouldBe` 
                Right (AstCall (AstSymbol ">") [AstInt 5, AstInt 3])

    describe "NewParser - If expressions" $ do
        it "parses simple if" $ do
            case parseExpression "if true: 1 else: 2" of
                Right (If cond thenBr elseBr) -> do
                    cond `shouldBe` AstBool True
                    thenBr `shouldBe` AstInt 1
                    elseBr `shouldBe` AstInt 2
                other -> expectationFailure $ "Unexpected result: " ++ show other
        
        it "parses if with comparison" $ do
            case parseExpression "if x < 10: x else: 0" of
                Right (If (AstCall (AstSymbol "<") [AstSymbol "x", AstInt 10]) thenBr elseBr) -> do
                    thenBr `shouldBe` AstSymbol "x"
                    elseBr `shouldBe` AstInt 0
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Lambda expressions" $ do
        it "parses simple lambda" $ do
            case parseExpression "fn(x): x * 2" of
                Right (Lambda ["x"] body) -> 
                    body `shouldBe` AstCall (AstSymbol "*") [AstSymbol "x", AstInt 2]
                other -> expectationFailure $ "Unexpected result: " ++ show other
        
        it "parses lambda with multiple params" $ do
            case parseExpression "fn(a, b): a + b" of
                Right (Lambda ["a", "b"] body) -> 
                    body `shouldBe` AstCall (AstSymbol "+") [AstSymbol "a", AstSymbol "b"]
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Function calls" $ do
        it "parses simple call" $ do
            parseExpression "add(1, 2)" `shouldBe` 
                Right (AstCall (AstSymbol "add") [AstInt 1, AstInt 2])
        
        it "parses call with no args" $ do
            parseExpression "foo()" `shouldBe` 
                Right (AstCall (AstSymbol "foo") [])
        
        it "parses nested calls" $ do
            parseExpression "add(mul(2, 3), 4)" `shouldBe` 
                Right (AstCall (AstSymbol "add") 
                    [AstCall (AstSymbol "mul") [AstInt 2, AstInt 3], 
                     AstInt 4])

    describe "NewParser - Strings" $ do
        it "parses string literal" $ do
            parseExpression "\"hello\"" `shouldBe` Right (AstString "hello")

    describe "NewParser - Lists" $ do
        it "parses empty list literal" $ do
            parseExpression "[]" `shouldBe` Right (AstList [])

        it "parses list literal with elements" $ do
            parseExpression "[1, 2, 3]" `shouldBe` Right (AstList [AstInt 1, AstInt 2, AstInt 3])

        it "parses list literal with trailing comma" $ do
            parseExpression "[1, 2, 3,]" `shouldBe` Right (AstList [AstInt 1, AstInt 2, AstInt 3])

        it "parses list in variable definition" $ do
            fmap stripAll (parseProgram "let liste = []") `shouldBe` Right [Define "liste" (AstList [])]

        it "parses indexing syntax" $ do
            parseExpression "xs[0]" `shouldBe`
                Right (AstCall (AstSymbol "get") [AstSymbol "xs", AstInt 0])

    describe "NewParser - Logical operators" $ do
        it "parses not expression" $ do
            parseExpression "!true" `shouldBe` 
                Right (AstCall (AstSymbol "not") [AstBool True])

    describe "NewParser - Power and factorial" $ do
        it "parses power operator" $ do
            parseExpression "2 ** 10" `shouldBe`
                Right (AstCall (AstSymbol "**") [AstInt 2, AstInt 10])

        it "parses postfix factorial" $ do
            parseExpression "5!" `shouldBe`
                Right (AstCall (AstSymbol "!") [AstInt 5])

    describe "NewParser - Comparison operators extended" $ do
        it "parses not equal" $ do
            parseExpression "5 != 3" `shouldBe` 
                Right (AstCall (AstSymbol "neq?") [AstInt 5, AstInt 3])
        
        it "parses less than or equal" $ do
            parseExpression "3 <= 5" `shouldBe` 
                Right (AstCall (AstSymbol "<=") [AstInt 3, AstInt 5])
        
        it "parses greater than or equal" $ do
            parseExpression "5 >= 3" `shouldBe` 
                Right (AstCall (AstSymbol ">=") [AstInt 5, AstInt 3])

    describe "NewParser - Unary minus" $ do
        it "parses negative number" $ do
            fmap stripAll (parseProgram "-42") `shouldBe` Right [AstInt (-42)]

    describe "NewParser - For loops" $ do
        it "parses simple for loop" $ do
            case fmap stripAll (parseProgram "for i in range(0, 10): print(i)") of
                Right [For "i" (AstInt 0) (AstInt 10) _] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other

        it "parses foreach loop" $ do
            case fmap stripAll (parseProgram "for x in [1, 2, 3]: print(x)") of
                Right [ForEach "x" (AstList [AstInt 1, AstInt 2, AstInt 3]) _] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - While loops" $ do
        it "parses simple while loop" $ do
            case fmap stripAll (parseProgram "while true: print(1)") of
                Right [While (AstBool True) _] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Return statement" $ do
        it "parses return statement" $ do
            case fmap stripAll (parseProgram "return 42") of
                Right [Return (AstInt 42)] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Print statement" $ do
        it "parses print statement" $ do
            case fmap stripAll (parseProgram "print(42)") of
                Right [Print [AstInt 42]] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other
        
        it "parses print with multiple args" $ do
            case fmap stripAll (parseProgram "print(1, 2, 3)") of
                Right [Print [AstInt 1, AstInt 2, AstInt 3]] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Break and Continue" $ do
        it "parses break statement" $ do
            case fmap stripAll (parseProgram "break") of
                Right [Break] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other
        
        it "parses continue statement" $ do
            case fmap stripAll (parseProgram "continue") of
                Right [Continue] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Assignment" $ do
        it "parses variable assignment" $ do
            case fmap stripAll (parseProgram "x = 42") of
                Right [Define "x" (AstInt 42)] -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other

    describe "NewParser - Lambda with no params" $ do
        it "parses lambda with no parameters" $ do
            case parseExpression "fn(): 42" of
                Right (Lambda [] (AstInt 42)) -> return ()
                other -> expectationFailure $ "Unexpected result: " ++ show other
