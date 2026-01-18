-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- SExprSpec.hs - Tests for SExpr module
--
module SExprSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SExpr

spec :: Spec
spec = do
    describe "SExpr data structure" $ do
        it "creates an integer SExpr" $ do
            let expr = SInt 42
            expr `shouldBe` SInt 42
        
        it "creates a symbol SExpr" $ do
            let expr = SSymbol "x"
            expr `shouldBe` SSymbol "x"
        
        it "creates a list SExpr" $ do
            let expr = SList [SInt 1, SInt 2]
            expr `shouldBe` SList [SInt 1, SInt 2]
    
    describe "getSymbol" $ do
        it "returns Just symbol for SSymbol" $ do
            getSymbol (SSymbol "test") `shouldBe` Just "test"
        
        it "returns Nothing for SInt" $ do
            getSymbol (SInt 42) `shouldBe` Nothing
        
        it "returns Nothing for SList" $ do
            getSymbol (SList []) `shouldBe` Nothing
    
    describe "getInteger" $ do
        it "returns Just integer for SInt" $ do
            getInteger (SInt 42) `shouldBe` Just 42
        
        it "returns Nothing for SSymbol" $ do
            getInteger (SSymbol "x") `shouldBe` Nothing
        
        it "returns Nothing for SList" $ do
            getInteger (SList []) `shouldBe` Nothing
    
    describe "getList" $ do
        it "returns Just list for SList" $ do
            let list = [SInt 1, SInt 2]
            getList (SList list) `shouldBe` Just list
        
        it "returns Nothing for SInt" $ do
            getList (SInt 42) `shouldBe` Nothing
        
        it "returns Nothing for SSymbol" $ do
            getList (SSymbol "x") `shouldBe` Nothing
    
    describe "printTree" $ do
        it "prints integer correctly" $ do
            printTree (SInt 5) `shouldBe` Just "a Number 5"
        
        it "prints negative integer" $ do
            printTree (SInt (-42)) `shouldBe` Just "a Number -42"
        
        it "prints zero" $ do
            printTree (SInt 0) `shouldBe` Just "a Number 0"
        
        it "prints symbol correctly" $ do
            printTree (SSymbol "x") `shouldBe` Just "a Symbol 'x'"
        
        it "prints operator symbols" $ do
            printTree (SSymbol "+") `shouldBe` Just "a Symbol '+'"
        
        it "prints empty list" $ do
            printTree (SList []) `shouldBe` Just "an empty List"
        
        it "prints simple list correctly" $ do
            let expr = SList [SSymbol "define", SSymbol "x", SInt 5]
            case printTree expr of
                Just str -> str `shouldContain` "List"
                Nothing -> expectationFailure "printTree returned Nothing"
        
        it "prints nested list correctly" $ do
            let expr = SList [SSymbol "+", SList [SSymbol "*", SInt 2, SInt 3], SInt 4]
            case printTree expr of
                Just str -> do
                    str `shouldContain` "List"
                    str `shouldContain` "Symbol '+'"
                Nothing -> expectationFailure "printTree returned Nothing"
    
    describe "SExpr edge cases" $ do
        it "handles large integers" $ do
            let expr = SInt 999999999
            expr `shouldBe` SInt 999999999
        
        it "handles empty symbol (degenerate)" $ do
            let expr = SSymbol ""
            getSymbol expr `shouldBe` Just ""
        
        it "handles nested empty lists" $ do
            let expr = SList [SList [], SList []]
            case getList expr of
                Just [SList [], SList []] -> return ()
                _ -> expectationFailure "Failed nested empty lists"
    
    describe "printList cases" $ do
        it "prints list with multiple elements" $ do
            let expr = SList [SInt 1, SInt 2, SInt 3]
            case printTree expr of
                Just str -> do
                    str `shouldContain` "Number 1"
                    str `shouldContain` "followed by"
                Nothing -> expectationFailure "printTree returned Nothing"
        
        it "prints list with single element" $ do
            let expr = SList [SInt 42]
            case printTree expr of
                Just str -> str `shouldContain` "Number 42"
                Nothing -> expectationFailure "printTree returned Nothing"

    describe "SExpr Show instance" $ do
        it "shows SInt" $ do
            show (SInt 42) `shouldContain` "SInt 42"
        
        it "shows SSymbol" $ do
            show (SSymbol "test") `shouldContain` "SSymbol"
            show (SSymbol "test") `shouldContain` "test"
        
        it "shows SList" $ do
            show (SList [SInt 1]) `shouldContain` "SList"

    describe "SExpr Eq instance" $ do
        it "compares SInt equality" $ do
            SInt 42 `shouldBe` SInt 42
            SInt 42 `shouldNotBe` SInt 0
        
        it "compares SSymbol equality" $ do
            SSymbol "x" `shouldBe` SSymbol "x"
            SSymbol "x" `shouldNotBe` SSymbol "y"
        
        it "compares SList equality" $ do
            SList [SInt 1] `shouldBe` SList [SInt 1]
            SList [SInt 1] `shouldNotBe` SList [SInt 2]
        
        it "different types are not equal" $ do
            SInt 1 `shouldNotBe` SSymbol "1"

    describe "SExpr - Accessor completeness" $ do
        it "getSymbol on all types" $ do
            getSymbol (SInt 1) `shouldBe` Nothing
            getSymbol (SSymbol "x") `shouldBe` Just "x"
            getSymbol (SList []) `shouldBe` Nothing
        
        it "getInteger on all types" $ do
            getInteger (SInt 42) `shouldBe` Just 42
            getInteger (SSymbol "x") `shouldBe` Nothing
            getInteger (SList []) `shouldBe` Nothing
        
        it "getList on all types" $ do
            getList (SInt 1) `shouldBe` Nothing
            getList (SSymbol "x") `shouldBe` Nothing
            getList (SList [SInt 1]) `shouldBe` Just [SInt 1]

    describe "printTree - Edge cases" $ do
        it "handles list with two elements" $ do
            let expr = SList [SInt 1, SInt 2]
            case printTree expr of
                Just str -> do
                    str `shouldContain` "Number 1"
                    str `shouldContain` "Number 2"
                    str `shouldContain` "followed by"
                Nothing -> expectationFailure "printTree returned Nothing"
        
        it "handles deeply nested list" $ do
            let expr = SList [SList [SList [SInt 1]]]
            case printTree expr of
                Just str -> str `shouldContain` "List"
                Nothing -> expectationFailure "printTree returned Nothing"
        
        it "handles symbol with special characters" $ do
            case printTree (SSymbol "!@#") of
                Just str -> str `shouldContain` "!@#"
                Nothing -> expectationFailure "printTree returned Nothing"
