{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- LexerSpec.hs
--}

module LexerSpec (spec) where

import Test.Hspec
import Lexer

getTokenTypes :: String -> Either String [TokenType]
getTokenTypes input = map tokenType <$> tokenize input

spec :: Spec
spec = do
    describe "Lexer - Basic tokens" $ do
        it "tokenizes integers" $ do
            getTokenTypes "42" `shouldBe` Right [TokInt, TokEOF]
        
        it "tokenizes negative integers" $ do
            getTokenTypes "-17" `shouldBe` Right [TokInt, TokEOF]
        
        it "tokenizes identifiers" $ do
            getTokenTypes "foo" `shouldBe` Right [TokIdent, TokEOF]
        
        it "tokenizes booleans" $ do
            getTokenTypes "true false" `shouldBe` Right [TokBool, TokBool, TokEOF]

    describe "Lexer - Keywords" $ do
        it "tokenizes 'let'" $ do
            getTokenTypes "let" `shouldBe` Right [TokLet, TokEOF]
        
        it "tokenizes 'def'" $ do
            getTokenTypes "def" `shouldBe` Right [TokDef, TokEOF]
        
        it "tokenizes 'fn'" $ do
            getTokenTypes "fn" `shouldBe` Right [TokFn, TokEOF]
        
        it "tokenizes 'if else'" $ do
            getTokenTypes "if else" `shouldBe` Right [TokIf, TokElse, TokEOF]

    describe "Lexer - Operators" $ do
        it "tokenizes arithmetic operators" $ do
            getTokenTypes "+ - * / %" `shouldBe` Right [TokPlus, TokMinus, TokMult, TokDiv, TokMod, TokEOF]
        
        it "tokenizes comparison operators" $ do
            getTokenTypes "== != < > <= >=" `shouldBe` Right [TokEq, TokNeq, TokLt, TokGt, TokLte, TokGte, TokEOF]
        
        it "tokenizes assignment and colon" $ do
            getTokenTypes "= :" `shouldBe` Right [TokAssign, TokColon, TokEOF]

        it "tokenizes power operator" $ do
            getTokenTypes "**" `shouldBe` Right [TokPow, TokEOF]

    describe "Lexer - Delimiters" $ do
        it "tokenizes parentheses" $ do
            getTokenTypes "( )" `shouldBe` Right [TokLParen, TokRParen, TokEOF]
        
        it "tokenizes brackets" $ do
            getTokenTypes "[ ]" `shouldBe` Right [TokLBracket, TokRBracket, TokEOF]
        
        it "tokenizes comma" $ do
            getTokenTypes "," `shouldBe` Right [TokComma, TokEOF]

    describe "Lexer - Complex expressions" $ do
        it "tokenizes variable definition" $ do
            getTokenTypes "let x = 42" `shouldBe` 
                Right [TokLet, TokIdent, TokAssign, TokInt, TokEOF]
        
        it "tokenizes function definition" $ do
            getTokenTypes "def add(a, b):" `shouldBe` 
                Right [TokDef, TokIdent, TokLParen, TokIdent, TokComma, TokIdent, TokRParen, TokColon, TokEOF]
        
        it "tokenizes arithmetic expression" $ do
            getTokenTypes "2 + 3 * 4" `shouldBe` 
                Right [TokInt, TokPlus, TokInt, TokMult, TokInt, TokEOF]
        
        it "tokenizes function call" $ do
            getTokenTypes "factorial(5)" `shouldBe` 
                Right [TokIdent, TokLParen, TokInt, TokRParen, TokEOF]

    describe "Lexer - Comments" $ do
        it "ignores line comments" $ do
            getTokenTypes "42 # this is a comment" `shouldBe` Right [TokInt, TokEOF]

    describe "Lexer - Error handling" $ do
        it "reports unexpected character" $ do
            case tokenize "@" of
                Left err -> err `shouldContain` "Unexpected"
                Right _ -> expectationFailure "Should have failed"

    describe "Lexer - Strings" $ do
        it "tokenizes simple string" $ do
            case tokenize "\"hello\"" of
                Right toks -> do
                    map tokenType toks `shouldBe` [TokString, TokEOF]
                    tokenValue (head toks) `shouldBe` "hello"
                Left err -> expectationFailure err
        
        it "tokenizes string with escape sequences" $ do
            case tokenize "\"hello\\nworld\"" of
                Right toks -> tokenValue (head toks) `shouldBe` "hello\nworld"
                Left err -> expectationFailure err
        
        it "tokenizes string with tab escape" $ do
            case tokenize "\"a\\tb\"" of
                Right toks -> tokenValue (head toks) `shouldBe` "a\tb"
                Left err -> expectationFailure err
        
        it "tokenizes string with backslash escape" $ do
            case tokenize "\"a\\\\b\"" of
                Right toks -> tokenValue (head toks) `shouldBe` "a\\b"
                Left err -> expectationFailure err
        
        it "tokenizes string with quote escape" $ do
            case tokenize "\"say \\\"hi\\\"\"" of
                Right toks -> tokenValue (head toks) `shouldBe` "say \"hi\""
                Left err -> expectationFailure err
        
        it "reports unterminated string" $ do
            case tokenize "\"unterminated" of
                Left err -> err `shouldContain` "Unterminated"
                Right _ -> expectationFailure "Should have failed"

    describe "Lexer - Additional keywords" $ do
        it "tokenizes 'elif'" $ do
            getTokenTypes "elif" `shouldBe` Right [TokElif, TokEOF]
        
        it "tokenizes 'return'" $ do
            getTokenTypes "return" `shouldBe` Right [TokReturn, TokEOF]
        
        it "tokenizes 'print'" $ do
            getTokenTypes "print" `shouldBe` Right [TokPrint, TokEOF]
        
        it "tokenizes 'for'" $ do
            getTokenTypes "for" `shouldBe` Right [TokFor, TokEOF]
        
        it "tokenizes 'while'" $ do
            getTokenTypes "while" `shouldBe` Right [TokWhile, TokEOF]
        
        it "tokenizes 'in'" $ do
            getTokenTypes "in" `shouldBe` Right [TokIn, TokEOF]
        
        it "tokenizes 'range'" $ do
            getTokenTypes "range" `shouldBe` Right [TokRange, TokEOF]
        
        it "tokenizes 'break'" $ do
            getTokenTypes "break" `shouldBe` Right [TokBreak, TokEOF]
        
        it "tokenizes 'continue'" $ do
            getTokenTypes "continue" `shouldBe` Right [TokContinue, TokEOF]

    describe "Lexer - Logical operators" $ do
        it "tokenizes 'and'" $ do
            getTokenTypes "and" `shouldBe` Right [TokAnd, TokEOF]
        
        it "tokenizes 'or'" $ do
            getTokenTypes "or" `shouldBe` Right [TokOr, TokEOF]
        
        it "tokenizes not operator" $ do
            getTokenTypes "!" `shouldBe` Right [TokNot, TokEOF]

    describe "Lexer - Braces" $ do
        it "tokenizes braces" $ do
            getTokenTypes "{ }" `shouldBe` Right [TokLBrace, TokRBrace, TokEOF]

    describe "Lexer - Newlines and indentation" $ do
        it "tokenizes newline" $ do
            case tokenize "a\nb" of
                Right toks -> TokNewline `elem` map tokenType toks `shouldBe` True
                Left err -> expectationFailure err

    describe "Lexer - Token positions" $ do
        it "tracks line number" $ do
            case tokenize "42" of
                Right (tok:_) -> tokenLine tok `shouldBe` 1
                _ -> expectationFailure "No token"
        
        it "tracks column number" $ do
            case tokenize "  42" of
                Right (tok:_) -> tokenCol tok `shouldBe` 3
                _ -> expectationFailure "No token"

    describe "Lexer - showToken" $ do
        it "shows token with position" $ do
            let tok = Token TokInt "42" 1 5
            showToken tok `shouldContain` "42"
            showToken tok `shouldContain` "1:5"

    describe "Lexer - Identifiers with special chars" $ do
        it "tokenizes identifier with question mark" $ do
            case tokenize "empty?" of
                Right (tok:_) -> do
                    tokenType tok `shouldBe` TokIdent
                    tokenValue tok `shouldBe` "empty?"
                _ -> expectationFailure "No token"
        
        it "tokenizes identifier with exclamation" $ do
            case tokenize "set!" of
                Right (tok:_) -> do
                    tokenType tok `shouldBe` TokIdent
                    tokenValue tok `shouldBe` "set!"
                _ -> expectationFailure "No token"
        
        it "tokenizes identifier with underscore" $ do
            case tokenize "my_var" of
                Right (tok:_) -> tokenValue tok `shouldBe` "my_var"
                _ -> expectationFailure "No token"

    describe "Lexer - Multiple lines" $ do
        it "handles multiple newlines" $ do
            case tokenize "a\n\n\nb" of
                Right toks -> do
                    let types = map tokenType toks
                    TokIdent `elem` types `shouldBe` True
                Left err -> expectationFailure err
        
        it "handles comment then newline" $ do
            case tokenize "# comment\n42" of
                Right toks -> map tokenType toks `shouldContain` [TokInt]
                Left err -> expectationFailure err
