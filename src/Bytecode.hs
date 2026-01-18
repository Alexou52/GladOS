{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Bytecode.hs
--}

module Bytecode (
    Instruction(..),
    Program,
    Value(..),
    showInstruction,
    showProgram,
    showValue
) where

data Value
    = VInt Int
    | VBool Bool
    | VString String
    | VList [Value]
    | VClosure [String] Program
    | VBuiltin String
    | VNil
    deriving (Eq)

instance Show Value where
    show = showValue

showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool True) = "true"
showValue (VBool False) = "false"
showValue (VString s) = s
showValue (VList vs) = "[" ++ intercalateComma (map showValue vs) ++ "]"
showValue (VClosure params _) = "<function/" ++ show (length params) ++ ">"
showValue (VBuiltin name) = "<builtin:" ++ name ++ ">"
showValue VNil = "nil"

intercalateComma :: [String] -> String
intercalateComma [] = ""
intercalateComma [x] = x
intercalateComma (x:xs) = x ++ ", " ++ intercalateComma xs

data Instruction
    = PUSH Value | POP | DUP | SWAP
    | LOAD String | STORE String
    | ADD | SUB | MUL | DIV | MOD | POW | FACT
    | EQ_ | NEQ | LT_ | GT_ | LTE | GTE
    | NOT | AND | OR | NEG | CONCAT
    | PRINT
    | MAKE_LIST Int | LIST_GET | LIST_LEN
    | JMP Int | JMP_IF_FALSE Int | JMP_IF_TRUE Int
    | BREAK | CONTINUE
    | CALL Int | RET
    | MAKE_CLOSURE [String] Program
    | NOP | HALT | LINE Int
    deriving (Eq)

instance Show Instruction where
    show = showInstruction

type Program = [Instruction]

showInstruction :: Instruction -> String
showInstruction (PUSH v) = "PUSH " ++ show v
showInstruction POP = "POP"
showInstruction DUP = "DUP"
showInstruction SWAP = "SWAP"
showInstruction (LOAD name) = "LOAD " ++ name
showInstruction (STORE name) = "STORE " ++ name
showInstruction ADD = "ADD"
showInstruction SUB = "SUB"
showInstruction MUL = "MUL"
showInstruction DIV = "DIV"
showInstruction MOD = "MOD"
showInstruction POW = "POW"
showInstruction FACT = "FACT"
showInstruction EQ_ = "EQ"
showInstruction NEQ = "NEQ"
showInstruction LT_ = "LT"
showInstruction GT_ = "GT"
showInstruction LTE = "LTE"
showInstruction GTE = "GTE"
showInstruction NOT = "NOT"
showInstruction AND = "AND"
showInstruction OR = "OR"
showInstruction NEG = "NEG"
showInstruction CONCAT = "CONCAT"
showInstruction PRINT = "PRINT"
showInstruction (MAKE_LIST n) = "MAKE_LIST " ++ show n
showInstruction LIST_GET = "LIST_GET"
showInstruction LIST_LEN = "LIST_LEN"
showInstruction (JMP n) = "JMP " ++ show n
showInstruction (JMP_IF_FALSE n) = "JMP_IF_FALSE " ++ show n
showInstruction (JMP_IF_TRUE n) = "JMP_IF_TRUE " ++ show n
showInstruction BREAK = "BREAK"
showInstruction CONTINUE = "CONTINUE"
showInstruction (CALL n) = "CALL " ++ show n
showInstruction RET = "RET"
showInstruction (MAKE_CLOSURE params _) = "MAKE_CLOSURE [" ++ unwords params ++ "]"
showInstruction NOP = "NOP"
showInstruction HALT = "HALT"
showInstruction (LINE n) = "LINE " ++ show n

showProgram :: Program -> String
showProgram prog = unlines $ zipWith fmt [(0::Int)..] prog
  where
    fmt n instr = pad 4 (show n) ++ ": " ++ showInstruction instr
    pad len s = replicate (len - length s) ' ' ++ s
