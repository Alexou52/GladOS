-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ReaderSpec.hs - Tests for Reader module
--
module ReaderSpec (spec) where

import Test.Hspec
import Reader
import SExpr

spec :: Spec
spec = do
    describe "parseSExpr - single expressions" $ do
        it "parses integer" $ do
            parseSExpr "42" `shouldBe` Right (SInt 42)
        
        it "parses negative integer" $ do
            parseSExpr "-10" `shouldBe` Right (SInt (-10))
        
        it "parses symbol" $ do
            parseSExpr "foo" `shouldBe` Right (SSymbol "foo")
        
        it "parses simple list" $ do
            case parseSExpr "(+ 1 2)" of
                Right (SList [SSymbol "+", SInt 1, SInt 2]) -> return ()
                _ -> expectationFailure "Failed to parse (+ 1 2)"
        
        it "parses nested list" $ do
            case parseSExpr "(* (+ 1 2) 3)" of
                Right (SList [SSymbol "*", SList [SSymbol "+", SInt 1, SInt 2], SInt 3]) -> return ()
                _ -> expectationFailure "Failed to parse nested list"
        
        it "rejects empty input" $ do
            case parseSExpr "" of
                Left _ -> return ()
                Right _ -> expectationFailure "Should reject empty input"
        
        it "rejects multiple expressions" $ do
            case parseSExpr "1 2" of
                Left _ -> return ()
                Right _ -> expectationFailure "Should reject multiple expressions"
    
    describe "parseManySExpr - multiple expressions" $ do
        it "parses empty string as empty list" $ do
            parseManySExpr "" `shouldSatisfy` either (const True) null
        
        it "parses single expression" $ do
            case parseManySExpr "42" of
                Right [SInt 42] -> return ()
                _ -> expectationFailure "Failed to parse single expr"
        
        it "parses multiple expressions" $ do
            case parseManySExpr "1 2 3" of
                Right [SInt 1, SInt 2, SInt 3] -> return ()
                _ -> expectationFailure "Failed to parse multiple exprs"
        
        it "parses mixed expressions" $ do
            case parseManySExpr "(+ 1 2) foo 42" of
                Right [SList _, SSymbol "foo", SInt 42] -> return ()
                _ -> expectationFailure "Failed to parse mixed exprs"
        
        it "handles comments" $ do
            case parseManySExpr "; comment\n42\n; another\n100" of
                Right [SInt 42, SInt 100] -> return ()
                _ -> expectationFailure "Failed to handle comments"
        
        it "handles inline comments" $ do
            case parseManySExpr "42 ; this is a comment\n100" of
                Right [SInt 42, SInt 100] -> return ()
                _ -> expectationFailure "Failed to handle inline comments"
    
    describe "parseManySExpr - list parsing" $ do
        it "parses empty list" $ do
            case parseManySExpr "()" of
                Right [SList []] -> return ()
                _ -> expectationFailure "Failed to parse empty list"
        
        it "parses list with spaces" $ do
            case parseManySExpr "(  +  1   2  )" of
                Right [SList [SSymbol "+", SInt 1, SInt 2]] -> return ()
                _ -> expectationFailure "Failed to parse spaced list"
        
        it "parses deeply nested lists" $ do
            case parseManySExpr "(a (b (c (d))))" of
                Right [SList _] -> return ()
                _ -> expectationFailure "Failed to parse deeply nested"
        
        it "rejects unterminated list" $ do
            case parseManySExpr "(+ 1 2" of
                Left _ -> return ()
                Right _ -> expectationFailure "Should reject unterminated list"
        
        it "rejects extra closing paren" $ do
            case parseManySExpr "(+ 1 2))" of
                Left _ -> return ()
                Right _ -> expectationFailure "Should reject extra paren"
    
    describe "parseManySExpr - atoms" $ do
        it "distinguishes integers from symbols" $ do
            case parseManySExpr "123 abc" of
                Right [SInt 123, SSymbol "abc"] -> return ()
                _ -> expectationFailure "Failed to distinguish int/symbol"
        
        it "parses zero" $ do
            parseManySExpr "0" `shouldBe` Right [SInt 0]
        
        it "parses symbols with special chars" $ do
            case parseManySExpr "+  -  *  /  <  >  ==" of
                Right [SSymbol "+", SSymbol "-", SSymbol "*", SSymbol "/", SSymbol "<", SSymbol ">", SSymbol "=="] -> return ()
                _ -> expectationFailure "Failed to parse operator symbols"
    
    describe "parseManySExpr - whitespace handling" $ do
        it "handles leading whitespace" $ do
            case parseManySExpr "   42" of
                Right [SInt 42] -> return ()
                _ -> expectationFailure "Failed with leading space"
        
        it "handles trailing whitespace" $ do
            case parseManySExpr "42   " of
                Right [SInt 42] -> return ()
                _ -> expectationFailure "Failed with trailing space"
        
        it "handles newlines" $ do
            case parseManySExpr "1\n2\n3" of
                Right [SInt 1, SInt 2, SInt 3] -> return ()
                _ -> expectationFailure "Failed with newlines"
        
        it "handles tabs" $ do
            case parseManySExpr "1\t2\t3" of
                Right [SInt 1, SInt 2, SInt 3] -> return ()
                _ -> expectationFailure "Failed with tabs"

    describe "parseManySExpr - Symbol parsing" $ do
        it "parses identifier with underscore" $ do
            case parseManySExpr "my_var" of
                Right [SSymbol "my_var"] -> return ()
                _ -> expectationFailure "Failed to parse underscore symbol"
        
        it "parses identifier with question mark" $ do
            case parseManySExpr "empty?" of
                Right [SSymbol "empty?"] -> return ()
                _ -> expectationFailure "Failed to parse ? symbol"
        
        it "parses long symbol names" $ do
            case parseManySExpr "this-is-a-long-symbol-name" of
                Right [SSymbol "this-is-a-long-symbol-name"] -> return ()
                _ -> expectationFailure "Failed to parse long symbol"

    describe "parseManySExpr - Complex lists" $ do
        it "parses list of symbols" $ do
            case parseManySExpr "(a b c d)" of
                Right [SList [SSymbol "a", SSymbol "b", SSymbol "c", SSymbol "d"]] -> return ()
                _ -> expectationFailure "Failed to parse symbol list"
        
        it "parses define expression" $ do
            case parseManySExpr "(define x 42)" of
                Right [SList [SSymbol "define", SSymbol "x", SInt 42]] -> return ()
                _ -> expectationFailure "Failed to parse define"
        
        it "parses lambda expression" $ do
            case parseManySExpr "(lambda (x) x)" of
                Right [SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "x"]] -> return ()
                _ -> expectationFailure "Failed to parse lambda"
        
        it "parses if expression" $ do
            case parseManySExpr "(if (< x 0) 0 x)" of
                Right [SList _] -> return ()
                _ -> expectationFailure "Failed to parse if"

    describe "parseManySExpr - Multiple programs" $ do
        it "parses multiple defines" $ do
            case parseManySExpr "(define x 1) (define y 2)" of
                Right [SList _, SList _] -> return ()
                _ -> expectationFailure "Failed to parse multiple defines"
        
        it "parses mixed expressions and atoms" $ do
            case parseManySExpr "42 (+ 1 2) foo" of
                Right [SInt 42, SList _, SSymbol "foo"] -> return ()
                _ -> expectationFailure "Failed to parse mixed"

    describe "parseSExpr - Edge cases" $ do
        it "rejects just whitespace" $ do
            case parseSExpr "   " of
                Left _ -> return ()
                Right _ -> expectationFailure "Should reject whitespace"
        
        it "parses expression with surrounding whitespace" $ do
            case parseSExpr "  42  " of
                Right (SInt 42) -> return ()
                _ -> expectationFailure "Failed with surrounding whitespace"
