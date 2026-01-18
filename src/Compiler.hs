{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Compiler.hs
--}

module Compiler (compile, compileProgram, compileExpr, compileStatement) where

import AST
import Bytecode

compile :: [Ast] -> Program
compile [] = [PUSH VNil, HALT]
compile asts = compileKeepLast asts ++ [HALT]

compileKeepLast :: [Ast] -> Program
compileKeepLast [] = []
compileKeepLast [ast] = compileLastStmt ast
compileKeepLast (ast:rest) = compileStatement ast ++ compileKeepLast rest

compileLastStmt :: Ast -> Program
compileLastStmt (Define name expr) = compileExpr expr ++ [STORE name]
compileLastStmt (Print args) = compilePrint args
compileLastStmt (Return expr) = compileExpr expr ++ [RET]
compileLastStmt (Located pos ast) = [LINE (posLine pos)] ++ compileLastStmt ast
compileLastStmt expr = compileExpr expr

compileProgram :: [Ast] -> Program
compileProgram = concatMap compileStatement

compileStatement :: Ast -> Program
compileStatement (Define name expr) = compileExpr expr ++ [STORE name]
compileStatement (Print args) = compilePrint args ++ [POP]
compileStatement (Return expr) = compileExpr expr ++ [RET]
compileStatement (For var start end body) = compileFor var start end body ++ [POP]
compileStatement (ForEach var iterable body) = compileForEach var iterable body ++ [POP]
compileStatement (While cond body) = compileWhile cond body ++ [POP]
compileStatement Break = [BREAK]
compileStatement Continue = [CONTINUE]
compileStatement (Located pos ast) = [LINE (posLine pos)] ++ compileStatement ast
compileStatement expr = compileExpr expr ++ [POP]

compileExpr :: Ast -> Program
compileExpr (AstInt n) = [PUSH (VInt n)]
compileExpr (AstBool b) = [PUSH (VBool b)]
compileExpr (AstString s) = [PUSH (VString s)]
compileExpr (AstList elems) = concatMap compileExpr elems ++ [MAKE_LIST (length elems)]
compileExpr (AstSymbol name) = [LOAD name]
compileExpr (Define name expr) = compileExpr expr ++ [STORE name, PUSH VNil]
compileExpr (Lambda params body) = [MAKE_CLOSURE params (compileBody body)]
compileExpr (If cond thenBr elseBr) = compileIf cond thenBr elseBr
compileExpr (For var start end body) = compileFor var start end body
compileExpr (ForEach var it body) = compileForEach var it body
compileExpr (While cond body) = compileWhile cond body
compileExpr Break = [BREAK]
compileExpr Continue = [CONTINUE]
compileExpr (AstCall (AstSymbol "append") [AstSymbol varName, val]) =
    -- Python-like: append(xs, x) mutates xs and returns nil
    [LOAD varName] ++ compileExpr val ++ [LOAD "append", CALL 2, STORE varName, PUSH VNil]
compileExpr (AstCall (AstSymbol "set") [AstSymbol varName, idx, val]) =
    -- Python-like: set(xs, i, x) mutates xs and returns nil
    [LOAD varName] ++ compileExpr idx ++ compileExpr val ++ [LOAD "set", CALL 3, STORE varName, PUSH VNil]
compileExpr (AstCall func args) = compileCall func args
compileExpr (Return expr) = compileExpr expr ++ [RET]
compileExpr (Print args) = compilePrint args
compileExpr (Block stmts) = compileBlock stmts
compileExpr (Located pos ast) = [LINE (posLine pos)] ++ compileExpr ast

compileBody :: Ast -> Program
compileBody (Block stmts) = compileBlockBody stmts ++ [RET]
compileBody (Return expr) = compileExpr expr ++ [RET]
compileBody expr = compileExpr expr ++ [RET]

compileBlock :: [Ast] -> Program
compileBlock [] = [PUSH VNil]
compileBlock stmts = compileBlockBody stmts

compileBlockBody :: [Ast] -> Program
compileBlockBody [] = [PUSH VNil]
compileBlockBody [stmt] = compileBlockStmt stmt
compileBlockBody (stmt:rest) = compileStatement stmt ++ compileBlockBody rest

compileBlockStmt :: Ast -> Program
compileBlockStmt (Located pos ast) = [LINE (posLine pos)] ++ compileBlockStmt ast
compileBlockStmt (Return expr) = compileExpr expr
compileBlockStmt (Define name expr) = compileExpr expr ++ [STORE name, PUSH VNil]
compileBlockStmt expr = compileExpr expr

compilePrint :: [Ast] -> Program
compilePrint args = concatMap compileExpr args ++ [PUSH (VInt (length args)), PRINT]

compileIf :: Ast -> Ast -> Ast -> Program
compileIf cond thenBr elseBr =
    let condCode = compileExpr cond
        thenCode = compileExpr thenBr
        elseCode = compileExpr elseBr
    in condCode ++ [JMP_IF_FALSE (length thenCode + 1)] ++ thenCode ++ [JMP (length elseCode)] ++ elseCode

-- for i in range(start, end): body
-- Compiles to:
--   i = start
--   loop_start:
--     if i >= end: goto loop_end
--     body
--     i = i + 1
--     goto loop_start
--   loop_end:
--     push nil
compileFor :: String -> Ast -> Ast -> Ast -> Program
compileFor var start end body =
    let startCode = compileExpr start ++ [STORE var]
        endCode = compileExpr end
        bodyCode = compileLoopBody body
        incrCode = [LOAD var, PUSH (VInt 1), ADD, STORE var]
        -- Check: if i >= end, exit loop
        checkCode = [LOAD var] ++ endCode ++ [GTE]
        checkLen = length checkCode
        bodyLen = length bodyCode
        incrLen = length incrCode
        -- JMP_IF_TRUE should skip: body + increment + back-jump
        exitOff = bodyLen + incrLen + 1
        -- JMP back should jump to the start of checkCode (right after startCode)
        backJump = -(checkLen + bodyLen + incrLen + 2)
    in startCode ++ checkCode ++ [JMP_IF_TRUE exitOff] ++
       resolveForLoopJumps bodyCode bodyLen incrLen ++
       incrCode ++ [JMP backJump, PUSH VNil]

-- for x in iterable: body
-- Compiles to:
--   __it = iterable
--   __i = 0
--   loop_start:
--     if __i >= len(__it): goto loop_end
--     x = get(__it, __i)
--     body
--     __i = __i + 1
--     goto loop_start
--   loop_end:
--     push nil
compileForEach :: String -> Ast -> Ast -> Program
compileForEach var iterable body =
    let itName = "__iter_" ++ var
        idxName = "__idx_" ++ var
        initCode = compileExpr iterable ++ [STORE itName] ++ [PUSH (VInt 0), STORE idxName]
        -- check: __idx >= len(__iter)
        lenCode = [LOAD itName, LOAD "len", CALL 1]
        checkCode = [LOAD idxName] ++ lenCode ++ [GTE]
        checkLen = length checkCode
        -- bind loop var
        bindCode = [LOAD itName, LOAD idxName, LOAD "get", CALL 2, STORE var]
        bodyCode = compileLoopBody body
        incrCode = [LOAD idxName, PUSH (VInt 1), ADD, STORE idxName]
        bodyLen = length (bindCode ++ bodyCode)
        incrLen = length incrCode
        exitOff = bodyLen + incrLen + 1
        backJump = -(checkLen + bodyLen + incrLen + 2)
        fullBody = bindCode ++ bodyCode
    in initCode ++ checkCode ++ [JMP_IF_TRUE exitOff] ++
       resolveForLoopJumps fullBody bodyLen incrLen ++
       incrCode ++ [JMP backJump, PUSH VNil]

-- while cond: body
compileWhile :: Ast -> Ast -> Program
compileWhile cond body =
    let condCode = compileExpr cond
        bodyCode = compileLoopBody body
        condLen = length condCode
        bodyLen = length bodyCode
        -- JMP_IF_FALSE should skip: body + JMP back
        exitOff = bodyLen + 1
        -- JMP back should jump to the start of condCode
        backJump = -(condLen + bodyLen + 2)
    in condCode ++ [JMP_IF_FALSE exitOff] ++
       resolveWhileLoopJumps bodyCode bodyLen ++
       [JMP backJump, PUSH VNil]

compileLoopBody :: Ast -> Program
compileLoopBody (Block stmts) = concatMap compileLoopStmt stmts
compileLoopBody expr = compileExpr expr ++ [POP]

compileLoopStmt :: Ast -> Program
compileLoopStmt Break = [BREAK]
compileLoopStmt Continue = [CONTINUE]
compileLoopStmt (Located pos ast) = [LINE (posLine pos)] ++ compileLoopStmt ast
compileLoopStmt stmt = compileStatement stmt

-- Replace BREAK and CONTINUE with actual jumps.
--
-- Offsets are computed relative to the *position inside the loop body*.
-- This avoids off-by-one bugs when the compiler adds extra instructions
-- after the body (like the back-jump or for-loop increment).
resolveWhileLoopJumps :: Program -> Int -> Program
resolveWhileLoopJumps prog bodyLen = go 0 prog
    where
        go _ [] = []
        -- BREAK should jump to just after the back-jump instruction.
        go idx (BREAK:rest) = JMP (bodyLen - idx) : go (idx + 1) rest
        -- CONTINUE should jump to the back-jump instruction.
        go idx (CONTINUE:rest) = JMP (bodyLen - idx - 1) : go (idx + 1) rest
        go idx (instr:rest) = instr : go (idx + 1) rest

resolveForLoopJumps :: Program -> Int -> Int -> Program
resolveForLoopJumps prog bodyLen incrLen = go 0 prog
    where
        go _ [] = []
        -- BREAK should jump to just after: increment + back-jump.
        go idx (BREAK:rest) = JMP (bodyLen - idx + incrLen) : go (idx + 1) rest
        -- CONTINUE should jump to the increment (skip remaining body).
        go idx (CONTINUE:rest) = JMP (bodyLen - idx - 1) : go (idx + 1) rest
        go idx (instr:rest) = instr : go (idx + 1) rest

compileCall :: Ast -> [Ast] -> Program
compileCall (AstSymbol op) args = case op of
    "+" -> binOp ADD args
    "-" -> subOp args
    "*" -> binOp MUL args
    "div" -> binOp DIV args
    "mod" -> binOp MOD args
    "**" -> binOp POW args
    "eq?" -> binOp EQ_ args
    "neq?" -> binOp NEQ args
    "<" -> binOp LT_ args
    ">" -> binOp GT_ args
    "<=" -> binOp LTE args
    ">=" -> binOp GTE args
    "not" -> unaryOp NOT args
    "!" -> unaryOp FACT args
    _ -> userCall (AstSymbol op) args
compileCall func args = userCall func args

binOp :: Instruction -> [Ast] -> Program
binOp op [a, b] = compileExpr a ++ compileExpr b ++ [op]
binOp _ _ = [PUSH VNil]

subOp :: [Ast] -> Program
subOp [a] = compileExpr a ++ [NEG]
subOp [a, b] = compileExpr a ++ compileExpr b ++ [SUB]
subOp _ = [PUSH VNil]

unaryOp :: Instruction -> [Ast] -> Program
unaryOp op [a] = compileExpr a ++ [op]
unaryOp _ _ = [PUSH VNil]

userCall :: Ast -> [Ast] -> Program
userCall func args = concatMap compileExpr args ++ compileExpr func ++ [CALL (length args)]
