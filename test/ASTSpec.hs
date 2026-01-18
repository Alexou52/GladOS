-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ASTSpec.hs - Tests for AST module
--
module ASTSpec (spec) where

import Test.Hspec
import AST

spec :: Spec
spec = do
    describe "AST Show instance" $ do
        it "shows AstInt correctly" $ do
            show (AstInt 42) `shouldBe` "42"
        
        it "shows negative AstInt" $ do
            show (AstInt (-10)) `shouldBe` "-10"
        
        it "shows AstBool True correctly" $ do
            show (AstBool True) `shouldBe` "true"
        
        it "shows AstBool False correctly" $ do
            show (AstBool False) `shouldBe` "false"
        
        it "shows Lambda as procedure" $ do
            let lambda = Lambda ["x", "y"] (AstInt 5)
            show lambda `shouldBe` "<function(x y)>"
        
        it "shows AstSymbol correctly" $ do
            show (AstSymbol "foo") `shouldBe` "foo"
        
        it "shows Define correctly" $ do
            let def = Define "x" (AstInt 10)
            show def `shouldContain` "let"
            show def `shouldContain` "x"
            show def `shouldContain` "10"
        
        it "shows AstCall correctly" $ do
            let call = AstCall (AstSymbol "+") [AstInt 1, AstInt 2]
            show call `shouldContain` "+"
            show call `shouldContain` "1"
            show call `shouldContain` "2"
        
        it "shows If correctly" $ do
            let ifExpr = If (AstBool True) (AstInt 1) (AstInt 2)
            show ifExpr `shouldContain` "if"
            show ifExpr `shouldContain` "true"
            show ifExpr `shouldContain` "1"
            show ifExpr `shouldContain` "2"
    
    describe "AST Eq instance" $ do
        it "compares AstInt equality" $ do
            AstInt 42 `shouldBe` AstInt 42
            AstInt 42 `shouldNotBe` AstInt 43
        
        it "compares AstBool equality" $ do
            AstBool True `shouldBe` AstBool True
            AstBool False `shouldBe` AstBool False
            AstBool True `shouldNotBe` AstBool False
        
        it "compares AstSymbol equality" $ do
            AstSymbol "x" `shouldBe` AstSymbol "x"
            AstSymbol "x" `shouldNotBe` AstSymbol "y"
        
        it "compares Lambda equality" $ do
            let lambda1 = Lambda ["x"] (AstInt 5)
            let lambda2 = Lambda ["x"] (AstInt 5)
            let lambda3 = Lambda ["y"] (AstInt 5)
            lambda1 `shouldBe` lambda2
            lambda1 `shouldNotBe` lambda3
    
    describe "AST constructors" $ do
        it "creates AstInt with intValue field" $ do
            let ast = AstInt { intValue = 100 }
            intValue ast `shouldBe` 100
        
        it "creates AstBool with boolValue field" $ do
            let ast = AstBool { boolValue = True }
            boolValue ast `shouldBe` True
        
        it "creates AstSymbol with symbolName field" $ do
            let ast = AstSymbol { symbolName = "test" }
            symbolName ast `shouldBe` "test"
        
        it "creates Lambda with lambdaParams and lambdaBody" $ do
            let ast = Lambda { lambdaParams = ["a", "b"], lambdaBody = AstInt 0 }
            lambdaParams ast `shouldBe` ["a", "b"]
            lambdaBody ast `shouldBe` AstInt 0
        
        it "creates Define with defName and defValue" $ do
            let ast = Define { defName = "x", defValue = AstInt 42 }
            defName ast `shouldBe` "x"
            defValue ast `shouldBe` AstInt 42
        
        it "creates If with all fields" $ do
            let ast = If { ifCondition = AstBool True
                         , ifThen = AstInt 1
                         , ifElse = AstInt 2 }
            ifCondition ast `shouldBe` AstBool True
            ifThen ast `shouldBe` AstInt 1
            ifElse ast `shouldBe` AstInt 2
        
        it "creates AstCall with callFunc and callArgs" $ do
            let ast = AstCall { callFunc = AstSymbol "+"
                              , callArgs = [AstInt 1, AstInt 2] }
            callFunc ast `shouldBe` AstSymbol "+"
            length (callArgs ast) `shouldBe` 2
    
    describe "AST nested structures" $ do
        it "handles deeply nested calls" $ do
            let nested = AstCall (AstSymbol "+") 
                            [AstCall (AstSymbol "*") [AstInt 2, AstInt 3],
                             AstInt 4]
            show nested `shouldContain` "+"
            show nested `shouldContain` "*"
        
        it "handles nested lambdas" $ do
            let innerLambda = Lambda ["y"] (AstInt 1)
            let outerLambda = Lambda ["x"] innerLambda
            show outerLambda `shouldBe` "<function(x)>"
        
        it "handles complex if expressions" $ do
            let complexIf = If (AstCall (AstSymbol "<") [AstInt 1, AstInt 2])
                               (AstCall (AstSymbol "+") [AstInt 1, AstInt 2])
                               (AstInt 0)
            show complexIf `shouldContain` "if"

    describe "AST - AstString" $ do
        it "shows AstString correctly" $ do
            show (AstString "hello") `shouldBe` "\"hello\""
        
        it "shows empty string" $ do
            show (AstString "") `shouldBe` "\"\""
        
        it "creates AstString with stringValue field" $ do
            let ast = AstString { stringValue = "test" }
            stringValue ast `shouldBe` "test"
        
        it "compares AstString equality" $ do
            AstString "abc" `shouldBe` AstString "abc"
            AstString "abc" `shouldNotBe` AstString "xyz"

    describe "AST - AstList" $ do
        it "shows empty list" $ do
            show (AstList []) `shouldBe` "[]"
        
        it "shows list with one element" $ do
            show (AstList [AstInt 42]) `shouldBe` "[42]"
        
        it "shows list with multiple elements" $ do
            show (AstList [AstInt 1, AstInt 2, AstInt 3]) `shouldBe` "[1, 2, 3]"
        
        it "creates AstList with listElements field" $ do
            let ast = AstList { listElements = [AstInt 1, AstInt 2] }
            length (listElements ast) `shouldBe` 2
        
        it "compares AstList equality" $ do
            AstList [AstInt 1] `shouldBe` AstList [AstInt 1]
            AstList [AstInt 1] `shouldNotBe` AstList [AstInt 2]

    describe "AST - For loop" $ do
        it "shows For correctly" $ do
            let forAst = For "i" (AstInt 0) (AstInt 10) (Block [AstInt 1])
            show forAst `shouldContain` "for"
            show forAst `shouldContain` "i"
            show forAst `shouldContain` "range"
        
        it "creates For with all fields" $ do
            let ast = For { forVar = "x", forStart = AstInt 0, forEnd = AstInt 5, forBody = Block [] }
            forVar ast `shouldBe` "x"
            forStart ast `shouldBe` AstInt 0
            forEnd ast `shouldBe` AstInt 5
        
        it "compares For equality" $ do
            let for1 = For "i" (AstInt 0) (AstInt 10) Break
            let for2 = For "i" (AstInt 0) (AstInt 10) Break
            let for3 = For "j" (AstInt 0) (AstInt 10) Break
            for1 `shouldBe` for2
            for1 `shouldNotBe` for3

    describe "AST - While loop" $ do
        it "shows While correctly" $ do
            let whileAst = While (AstBool True) (Block [AstInt 1])
            show whileAst `shouldContain` "while"
        
        it "creates While with all fields" $ do
            let ast = While { whileCond = AstBool False, whileBody = Break }
            whileCond ast `shouldBe` AstBool False
            whileBody ast `shouldBe` Break
        
        it "compares While equality" $ do
            let w1 = While (AstBool True) Break
            let w2 = While (AstBool True) Break
            let w3 = While (AstBool False) Break
            w1 `shouldBe` w2
            w1 `shouldNotBe` w3

    describe "AST - Break and Continue" $ do
        it "shows Break correctly" $ do
            show Break `shouldBe` "break"
        
        it "shows Continue correctly" $ do
            show Continue `shouldBe` "continue"
        
        it "compares Break equality" $ do
            Break `shouldBe` Break
        
        it "compares Continue equality" $ do
            Continue `shouldBe` Continue
        
        it "Break and Continue are different" $ do
            Break `shouldNotBe` Continue

    describe "AST - Return" $ do
        it "shows Return correctly" $ do
            show (Return (AstInt 42)) `shouldBe` "return 42"
        
        it "creates Return with returnValue field" $ do
            let ast = Return { returnValue = AstBool True }
            returnValue ast `shouldBe` AstBool True
        
        it "compares Return equality" $ do
            Return (AstInt 1) `shouldBe` Return (AstInt 1)
            Return (AstInt 1) `shouldNotBe` Return (AstInt 2)

    describe "AST - Block" $ do
        it "shows Block correctly" $ do
            let block = Block [AstInt 1, AstInt 2]
            show block `shouldContain` "1"
            show block `shouldContain` "2"
        
        it "creates Block with blockStmts field" $ do
            let ast = Block { blockStmts = [AstInt 1, AstInt 2] }
            length (blockStmts ast) `shouldBe` 2
        
        it "compares Block equality" $ do
            Block [AstInt 1] `shouldBe` Block [AstInt 1]
            Block [AstInt 1] `shouldNotBe` Block [AstInt 2]

    describe "AST - Print" $ do
        it "shows Print correctly" $ do
            let printAst = Print [AstInt 42, AstString "hi"]
            show printAst `shouldContain` "print"
            show printAst `shouldContain` "42"
        
        it "creates Print with printArgs field" $ do
            let ast = Print { printArgs = [AstInt 1] }
            length (printArgs ast) `shouldBe` 1
        
        it "compares Print equality" $ do
            Print [AstInt 1] `shouldBe` Print [AstInt 1]
            Print [AstInt 1] `shouldNotBe` Print [AstInt 2]

    describe "AST - Located" $ do
        it "shows Located unwrapped" $ do
            let loc = Located (SourcePos 1 1) (AstInt 42)
            show loc `shouldBe` "42"
        
        it "creates Located with locPos and locAst fields" $ do
            let pos = SourcePos 5 10
            let ast = Located { locPos = pos, locAst = AstBool True }
            posLine (locPos ast) `shouldBe` 5
            posCol (locPos ast) `shouldBe` 10
            locAst ast `shouldBe` AstBool True
        
        it "compares Located equality" $ do
            let loc1 = Located (SourcePos 1 1) (AstInt 42)
            let loc2 = Located (SourcePos 1 1) (AstInt 42)
            let loc3 = Located (SourcePos 2 2) (AstInt 42)
            loc1 `shouldBe` loc2
            loc1 `shouldNotBe` loc3

    describe "AST - SourcePos" $ do
        it "creates SourcePos correctly" $ do
            let pos = SourcePos 10 20
            posLine pos `shouldBe` 10
            posCol pos `shouldBe` 20
        
        it "compares SourcePos equality" $ do
            SourcePos 1 2 `shouldBe` SourcePos 1 2
            SourcePos 1 2 `shouldNotBe` SourcePos 1 3
        
        it "shows SourcePos" $ do
            show (SourcePos 5 10) `shouldContain` "5"

    describe "AST - joinComma helper" $ do
        it "handles empty list in AstList" $ do
            show (AstList []) `shouldBe` "[]"
        
        it "handles single element list" $ do
            show (AstList [AstBool True]) `shouldBe` "[true]"
