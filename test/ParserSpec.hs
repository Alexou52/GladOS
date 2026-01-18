-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ParserSpec.hs - Tests for Parser module
--
module ParserSpec (spec) where

import Test.Hspec
import SExpr
import AST
import Parser

spec :: Spec
spec = do
    describe "sexprToAST - Basic types" $ do
        it "parses integer" $ do
            sexprToAST (SInt 42) `shouldBe` Just (AstInt 42)
        
        it "parses symbol" $ do
            sexprToAST (SSymbol "x") `shouldBe` Just (AstSymbol "x")
        
        it "parses boolean true" $ do
            sexprToAST (SSymbol "#t") `shouldBe` Just (AstBool True)
        
        it "parses boolean false" $ do
            sexprToAST (SSymbol "#f") `shouldBe` Just (AstBool False)
    
    describe "sexprToAST - Define special form" $ do
        it "parses valid define" $ do
            let expr = SList [SSymbol "define", SSymbol "x", SInt 5]
            case sexprToAST expr of
                Just (Define name value) -> do
                    name `shouldBe` "x"
                    value `shouldBe` AstInt 5
                _ -> expectationFailure "Failed to parse define"
        
        it "rejects invalid define with extra arguments" $ do
            let expr = SList [SSymbol "define", SSymbol "x", SInt 5, SInt 6]
            sexprToAST expr `shouldBe` Nothing
        
        it "rejects define with missing arguments" $ do
            let expr = SList [SSymbol "define", SSymbol "x"]
            sexprToAST expr `shouldBe` Nothing
    
    describe "sexprToAST - Function calls" $ do
        it "parses simple function call" $ do
            let expr = SList [SSymbol "+", SInt 2, SInt 3]
            case sexprToAST expr of
                Just (AstCall func args) -> do
                    func `shouldBe` AstSymbol "+"
                    length args `shouldBe` 2
                _ -> expectationFailure "Failed to parse function call"
        
        it "parses nested function calls" $ do
            let expr = SList [SSymbol "+", SSymbol "x", SList [SSymbol "*", SInt 4, SSymbol "y"]]
            case sexprToAST expr of
                Just (AstCall _ args) -> do
                    length args `shouldBe` 2
                    case args !! 1 of
                        AstCall innerFunc innerArgs -> do
                            innerFunc `shouldBe` AstSymbol "*"
                            length innerArgs `shouldBe` 2
                        _ -> expectationFailure "Second argument should be a function call"
                _ -> expectationFailure "Failed to parse nested call"
        
        it "parses define with function call" $ do
            let expr = SList [SSymbol "define", SSymbol "fourtyTwo", SList [SSymbol "*", SInt 7, SInt 6]]
            case sexprToAST expr of
                Just (Define name (AstCall func args)) -> do
                    name `shouldBe` "fourtyTwo"
                    func `shouldBe` AstSymbol "*"
                    length args `shouldBe` 2
                _ -> expectationFailure "Failed to parse define with call"
    
    describe "sexprToAST - If expressions" $ do
        it "parses if expression" $ do
            let expr = SList [SSymbol "if", SList [SSymbol ">", SSymbol "x", SInt 4], SInt 1, SInt 0]
            case sexprToAST expr of
                Just (If _ thenExpr elseExpr) -> do
                    thenExpr `shouldBe` AstInt 1
                    elseExpr `shouldBe` AstInt 0
                _ -> expectationFailure "Failed to parse if"
    
    describe "sexprToAST - Lambda expressions" $ do
        it "parses lambda with no args" $ do
            let expr = SList [SSymbol "lambda", SList [], SInt 42]
            case sexprToAST expr of
                Just (Lambda params body) -> do
                    params `shouldBe` []
                    body `shouldBe` AstInt 42
                _ -> expectationFailure "Failed to parse lambda"
        
        it "parses lambda with single arg" $ do
            let expr = SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "x"]
            case sexprToAST expr of
                Just (Lambda params _) -> params `shouldBe` ["x"]
                _ -> expectationFailure "Failed to parse lambda"
        
        it "parses lambda with multiple args" $ do
            let expr = SList [SSymbol "lambda", SList [SSymbol "x", SSymbol "y"], SList [SSymbol "+", SSymbol "x", SSymbol "y"]]
            case sexprToAST expr of
                Just (Lambda params _) -> params `shouldBe` ["x", "y"]
                _ -> expectationFailure "Failed to parse lambda"
    
    describe "sexprToAST - Boolean keywords" $ do
        it "parses #t as true" $ do
            sexprToAST (SSymbol "#t") `shouldBe` Just (AstBool True)
        
        it "parses #f as false" $ do
            sexprToAST (SSymbol "#f") `shouldBe` Just (AstBool False)
        
        it "parses 'true' as true" $ do
            sexprToAST (SSymbol "true") `shouldBe` Just (AstBool True)
        
        it "parses 'false' as false" $ do
            sexprToAST (SSymbol "false") `shouldBe` Just (AstBool False)
    
    describe "sexprToAST - Edge cases" $ do
        it "rejects empty list" $ do
            sexprToAST (SList []) `shouldBe` Nothing
        
        it "parses single-element list as function call with no args" $ do
            let expr = SList [SSymbol "f"]
            case sexprToAST expr of
                Just (AstCall func args) -> do
                    func `shouldBe` AstSymbol "f"
                    args `shouldBe` []
                _ -> expectationFailure "Failed single-element list"
        
        it "handles deeply nested calls" $ do
            let expr = SList [SSymbol "+", SList [SSymbol "+", SList [SSymbol "+", SInt 1, SInt 2], SInt 3], SInt 4]
            case sexprToAST expr of
                Just (AstCall _ _) -> return ()
                _ -> expectationFailure "Failed deeply nested"
        
        it "rejects define with non-symbol name" $ do
            let expr = SList [SSymbol "define", SInt 42, SInt 5]
            sexprToAST expr `shouldBe` Nothing
    
    describe "sexprToAST - Function definition syntactic sugar" $ do
        it "parses (define (func args) body) as syntactic sugar" $ do
            let expr = SList [SSymbol "define", SList [SSymbol "add", SSymbol "a", SSymbol "b"], 
                              SList [SSymbol "+", SSymbol "a", SSymbol "b"]]
            case sexprToAST expr of
                Just (Define name (Lambda params body)) -> do
                    name `shouldBe` "add"
                    params `shouldBe` ["a", "b"]
                    case body of
                        AstCall func args -> do
                            func `shouldBe` AstSymbol "+"
                            length args `shouldBe` 2
                        _ -> expectationFailure "Body should be a call"
                _ -> expectationFailure "Failed to parse function definition sugar"
        
        it "parses (define (func) body) with no args" $ do
            let expr = SList [SSymbol "define", SList [SSymbol "getFortyTwo"], SInt 42]
            case sexprToAST expr of
                Just (Define name (Lambda params body)) -> do
                    name `shouldBe` "getFortyTwo"
                    params `shouldBe` []
                    body `shouldBe` AstInt 42
                _ -> expectationFailure "Failed no-arg function"
        
        it "parses recursive function definition" $ do
            let expr = SList [SSymbol "define", 
                              SList [SSymbol "fact", SSymbol "n"],
                              SList [SSymbol "if", 
                                    SList [SSymbol "eq?", SSymbol "n", SInt 1],
                                    SInt 1,
                                    SList [SSymbol "*", SSymbol "n", 
                                          SList [SSymbol "fact", 
                                                SList [SSymbol "-", SSymbol "n", SInt 1]]]]]
            case sexprToAST expr of
                Just (Define name (Lambda params _)) -> do
                    name `shouldBe` "fact"
                    params `shouldBe` ["n"]
                _ -> expectationFailure "Failed recursive function"
    
    describe "sexprToAST - If expression variations" $ do
        it "parses if with missing branches as Nothing" $ do
            let expr = SList [SSymbol "if", SSymbol "#t", SInt 1]
            sexprToAST expr `shouldBe` Nothing
        
        it "parses if with extra arguments as Nothing" $ do
            let expr = SList [SSymbol "if", SSymbol "#t", SInt 1, SInt 2, SInt 3]
            sexprToAST expr `shouldBe` Nothing
    
    describe "sexprToAST - Lambda variations" $ do
        it "rejects lambda with non-list params" $ do
            let expr = SList [SSymbol "lambda", SSymbol "x", SInt 1]
            sexprToAST expr `shouldBe` Nothing
        
        it "rejects lambda with non-symbol in params" $ do
            let expr = SList [SSymbol "lambda", SList [SInt 42], SInt 1]
            sexprToAST expr `shouldBe` Nothing
        
        it "rejects lambda with missing body" $ do
            let expr = SList [SSymbol "lambda", SList [SSymbol "x"]]
            sexprToAST expr `shouldBe` Nothing
