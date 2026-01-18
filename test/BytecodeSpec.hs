{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- BytecodeSpec.hs
--}

module BytecodeSpec (spec) where

import Test.Hspec
import Bytecode

spec :: Spec
spec = do
    describe "Bytecode - Value Show" $ do
        it "shows integer value" $ do
            show (VInt 42) `shouldBe` "42"
        
        it "shows negative integer" $ do
            show (VInt (-5)) `shouldBe` "-5"
        
        it "shows true boolean" $ do
            show (VBool True) `shouldBe` "true"
        
        it "shows false boolean" $ do
            show (VBool False) `shouldBe` "false"
        
        it "shows nil" $ do
            show VNil `shouldBe` "nil"
        
        it "shows builtin" $ do
            show (VBuiltin "+") `shouldBe` "<builtin:+>"
        
        it "shows closure" $ do
            show (VClosure ["x", "y"] []) `shouldBe` "<function/2>"

    describe "Bytecode - Instruction Show" $ do
        it "shows PUSH" $ do
            showInstruction (PUSH (VInt 42)) `shouldBe` "PUSH 42"
        
        it "shows arithmetic ops" $ do
            showInstruction ADD `shouldBe` "ADD"
            showInstruction SUB `shouldBe` "SUB"
            showInstruction MUL `shouldBe` "MUL"
            showInstruction DIV `shouldBe` "DIV"
            showInstruction MOD `shouldBe` "MOD"
        
        it "shows comparison ops" $ do
            showInstruction EQ_ `shouldBe` "EQ"
            showInstruction NEQ `shouldBe` "NEQ"
            showInstruction LT_ `shouldBe` "LT"
            showInstruction GT_ `shouldBe` "GT"
        
        it "shows stack ops" $ do
            showInstruction POP `shouldBe` "POP"
            showInstruction DUP `shouldBe` "DUP"
            showInstruction SWAP `shouldBe` "SWAP"
        
        it "shows variable ops" $ do
            showInstruction (LOAD "x") `shouldBe` "LOAD x"
            showInstruction (STORE "y") `shouldBe` "STORE y"
        
        it "shows jump ops" $ do
            showInstruction (JMP 10) `shouldBe` "JMP 10"
            showInstruction (JMP_IF_FALSE 5) `shouldBe` "JMP_IF_FALSE 5"
            showInstruction (JMP_IF_TRUE 3) `shouldBe` "JMP_IF_TRUE 3"
        
        it "shows call/ret" $ do
            showInstruction (CALL 2) `shouldBe` "CALL 2"
            showInstruction RET `shouldBe` "RET"
        
        it "shows closure" $ do
            showInstruction (MAKE_CLOSURE ["a", "b"] [ADD, RET]) `shouldBe` "MAKE_CLOSURE [a b]"

    describe "Bytecode - Program Show" $ do
        it "shows program with line numbers" $ do
            let prog = [PUSH (VInt 1), PUSH (VInt 2), ADD, HALT]
            let output = showProgram prog
            output `shouldContain` "0: PUSH 1"
            output `shouldContain` "1: PUSH 2"
            output `shouldContain` "2: ADD"
            output `shouldContain` "3: HALT"

    describe "Bytecode - Value Equality" $ do
        it "compares integers" $ do
            VInt 42 `shouldBe` VInt 42
            VInt 42 `shouldNotBe` VInt 0
        
        it "compares booleans" $ do
            VBool True `shouldBe` VBool True
            VBool True `shouldNotBe` VBool False
        
        it "compares nil" $ do
            VNil `shouldBe` VNil
        
        it "compares builtins" $ do
            VBuiltin "+" `shouldBe` VBuiltin "+"
            VBuiltin "+" `shouldNotBe` VBuiltin "-"

    describe "Bytecode - Instruction Equality" $ do
        it "compares PUSH instructions" $ do
            PUSH (VInt 1) `shouldBe` PUSH (VInt 1)
            PUSH (VInt 1) `shouldNotBe` PUSH (VInt 2)
        
        it "compares JMP instructions" $ do
            JMP 10 `shouldBe` JMP 10
            JMP 10 `shouldNotBe` JMP 20
        
        it "compares simple instructions" $ do
            ADD `shouldBe` ADD
            ADD `shouldNotBe` SUB

    describe "Bytecode - Additional Value types" $ do
        it "shows string value" $ do
            show (VString "test") `shouldBe` "test"
        
        it "shows empty list" $ do
            show (VList []) `shouldBe` "[]"
        
        it "shows list with elements" $ do
            show (VList [VInt 1, VInt 2]) `shouldBe` "[1, 2]"
        
        it "shows nested list" $ do
            show (VList [VList [VInt 1], VInt 2]) `shouldBe` "[[1], 2]"
        
        it "compares string values" $ do
            VString "a" `shouldBe` VString "a"
            VString "a" `shouldNotBe` VString "b"
        
        it "compares list values" $ do
            VList [VInt 1] `shouldBe` VList [VInt 1]
            VList [VInt 1] `shouldNotBe` VList [VInt 2]
        
        it "compares closure values" $ do
            VClosure ["x"] [ADD] `shouldBe` VClosure ["x"] [ADD]
            VClosure ["x"] [ADD] `shouldNotBe` VClosure ["y"] [ADD]

    describe "Bytecode - Additional Instruction Show" $ do
        it "shows LTE" $ do
            showInstruction LTE `shouldBe` "LTE"
        
        it "shows GTE" $ do
            showInstruction GTE `shouldBe` "GTE"
        
        it "shows NEG" $ do
            showInstruction NEG `shouldBe` "NEG"
        
        it "shows CONCAT" $ do
            showInstruction CONCAT `shouldBe` "CONCAT"
        
        it "shows PRINT" $ do
            showInstruction PRINT `shouldBe` "PRINT"
        
        it "shows LIST_GET" $ do
            showInstruction LIST_GET `shouldBe` "LIST_GET"
        
        it "shows LIST_LEN" $ do
            showInstruction LIST_LEN `shouldBe` "LIST_LEN"
        
        it "shows BREAK" $ do
            showInstruction BREAK `shouldBe` "BREAK"
        
        it "shows CONTINUE" $ do
            showInstruction CONTINUE `shouldBe` "CONTINUE"
        
        it "shows HALT" $ do
            showInstruction HALT `shouldBe` "HALT"
        
        it "shows LINE" $ do
            showInstruction (LINE 42) `shouldBe` "LINE 42"

    describe "Bytecode - Instruction Equality extended" $ do
        it "compares LOAD instructions" $ do
            LOAD "x" `shouldBe` LOAD "x"
            LOAD "x" `shouldNotBe` LOAD "y"
        
        it "compares STORE instructions" $ do
            STORE "a" `shouldBe` STORE "a"
            STORE "a" `shouldNotBe` STORE "b"
        
        it "compares CALL instructions" $ do
            CALL 2 `shouldBe` CALL 2
            CALL 2 `shouldNotBe` CALL 3
        
        it "compares MAKE_LIST instructions" $ do
            MAKE_LIST 3 `shouldBe` MAKE_LIST 3
            MAKE_LIST 3 `shouldNotBe` MAKE_LIST 4
        
        it "compares MAKE_CLOSURE instructions" $ do
            MAKE_CLOSURE ["x"] [ADD] `shouldBe` MAKE_CLOSURE ["x"] [ADD]
            MAKE_CLOSURE ["x"] [ADD] `shouldNotBe` MAKE_CLOSURE ["y"] [ADD]
        
        it "compares LINE instructions" $ do
            LINE 10 `shouldBe` LINE 10
            LINE 10 `shouldNotBe` LINE 20
        
        it "compares JMP_IF_TRUE instructions" $ do
            JMP_IF_TRUE 5 `shouldBe` JMP_IF_TRUE 5
            JMP_IF_TRUE 5 `shouldNotBe` JMP_IF_TRUE 10

    describe "Bytecode - showProgram format" $ do
        it "shows empty program" $ do
            showProgram [] `shouldBe` ""
        
        it "shows single instruction" $ do
            let output = showProgram [HALT]
            output `shouldContain` "HALT"
        
        it "numbers instructions correctly" $ do
            let output = showProgram [PUSH (VInt 1), ADD, HALT]
            output `shouldContain` "0:"
            output `shouldContain` "1:"
            output `shouldContain` "2:"
