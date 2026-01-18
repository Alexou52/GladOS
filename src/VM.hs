{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- VM.hs
--}

module VM (VMState(..), VMResult, runVM, runProgram, runProgramWithArgs, initialState, initialStateWithArgs, VMOutput(..)) where

import qualified Data.Map as Map
import Bytecode

data VMOutput = VMOutput { outputLines :: [String], outputValue :: Value }
    deriving (Show, Eq)

data VMState = VMState
    { stack :: [Value]
    , env :: Map.Map String Value
    , code :: Program
    , pc :: Int
    , callStack :: [(Int, Map.Map String Value, Program)]
    , halted :: Bool
    , output :: [String]
    , currentLine :: Int
    } deriving (Show, Eq)

type VMResult = Either String VMOutput

initialState :: Program -> VMState
initialState = initialStateWithArgs []

initialStateWithArgs :: [String] -> Program -> VMState
initialStateWithArgs args prog = VMState [] (builtinEnvWithArgs args) prog 0 [] False [] 1

builtinEnvWithArgs :: [String] -> Map.Map String Value
builtinEnvWithArgs args = Map.fromList
    [ ("+", VBuiltin "+"), ("-", VBuiltin "-"), ("*", VBuiltin "*")
    , ("div", VBuiltin "div"), ("mod", VBuiltin "mod")
    , ("eq?", VBuiltin "eq?"), ("<", VBuiltin "<"), (">", VBuiltin ">")
    , ("len", VBuiltin "len"), ("get", VBuiltin "get")
    , ("append", VBuiltin "append"), ("set", VBuiltin "set"), ("concat", VBuiltin "concat")
    , ("argv", VList (map VString args))
    , ("argc", VInt (length args))
    ]

runProgram :: Program -> VMResult
runProgram = runProgramWithArgs []

runProgramWithArgs :: [String] -> Program -> VMResult
runProgramWithArgs args = runVM . initialStateWithArgs args

runVM :: VMState -> VMResult
runVM st
    | halted st = getResult st
    | pc st >= length (code st) = getResult st
    | otherwise = step st >>= runVM

getResult :: VMState -> VMResult
getResult st = Right $ VMOutput (reverse $ output st) (maybe VNil id $ safeHead $ stack st)
  where safeHead [] = Nothing; safeHead (x:_) = Just x

step :: VMState -> Either String VMState
step st = exec (code st !! pc st) st

exec :: Instruction -> VMState -> Either String VMState
exec (PUSH v) st = Right st { stack = v : stack st, pc = pc st + 1 }
exec POP st = case stack st of
    (_:r) -> Right st { stack = r, pc = pc st + 1 }
    [] -> Left "POP: empty stack"
exec DUP st = case stack st of
    (v:_) -> Right st { stack = v : stack st, pc = pc st + 1 }
    [] -> Left "DUP: empty stack"
exec SWAP st = case stack st of
    (a:b:r) -> Right st { stack = b:a:r, pc = pc st + 1 }
    _ -> Left "SWAP: insufficient stack"
exec (LOAD name) st = case Map.lookup name (env st) of
    Just v -> Right st { stack = v : stack st, pc = pc st + 1 }
    Nothing -> Left $ "line " ++ show (currentLine st) ++ ": undefined variable '" ++ name ++ "'"
exec (STORE name) st = case stack st of
    (v:r) -> Right st { stack = r, env = Map.insert name v (env st), pc = pc st + 1 }
    [] -> Left "STORE: empty stack"
exec ADD st = intBinOp (+) "ADD" st
exec SUB st = intBinOp (-) "SUB" st
exec MUL st = intBinOp (*) "MUL" st
exec DIV st = case stack st of
    (VInt 0:_:_) -> Left "DIV: division by zero"
    (VInt b:VInt a:r) -> Right st { stack = VInt (a `div` b) : r, pc = pc st + 1 }
    _ -> Left "DIV: invalid operands"
exec MOD st = case stack st of
    (VInt 0:_:_) -> Left "MOD: division by zero"
    (VInt b:VInt a:r) -> Right st { stack = VInt (a `mod` b) : r, pc = pc st + 1 }
    _ -> Left "MOD: invalid operands"
exec POW st = case stack st of
    (VInt b:VInt a:r)
        | b < 0 -> Left "POW: negative exponent"
        | otherwise -> Right st { stack = VInt (a ^ b) : r, pc = pc st + 1 }
    _ -> Left "POW: invalid operands"
exec FACT st = case stack st of
    (VInt n:r)
        | n < 0 -> Left "FACT: negative input"
        | otherwise -> Right st { stack = VInt (fact n) : r, pc = pc st + 1 }
    _ -> Left "FACT: invalid operand"
exec EQ_ st = case stack st of
    (VInt b:VInt a:r) -> Right st { stack = VBool (a == b) : r, pc = pc st + 1 }
    (VBool b:VBool a:r) -> Right st { stack = VBool (a == b) : r, pc = pc st + 1 }
    _ -> Left "EQ: invalid operands"
exec NEQ st = case stack st of
    (VInt b:VInt a:r) -> Right st { stack = VBool (a /= b) : r, pc = pc st + 1 }
    (VBool b:VBool a:r) -> Right st { stack = VBool (a /= b) : r, pc = pc st + 1 }
    _ -> Left "NEQ: invalid operands"
exec LT_ st = cmpOp (<) "LT" st
exec GT_ st = cmpOp (>) "GT" st
exec LTE st = cmpOp (<=) "LTE" st
exec GTE st = cmpOp (>=) "GTE" st
exec NOT st = case stack st of
    (VBool b:r) -> Right st { stack = VBool (not b) : r, pc = pc st + 1 }
    _ -> Left "NOT: expected boolean"
exec AND st = case stack st of
    (VBool b:VBool a:r) -> Right st { stack = VBool (a && b) : r, pc = pc st + 1 }
    _ -> Left "AND: expected booleans"
exec OR st = case stack st of
    (VBool b:VBool a:r) -> Right st { stack = VBool (a || b) : r, pc = pc st + 1 }
    _ -> Left "OR: expected booleans"
exec NEG st = case stack st of
    (VInt n:r) -> Right st { stack = VInt (-n) : r, pc = pc st + 1 }
    _ -> Left "NEG: expected integer"
exec (JMP off) st = Right st { pc = pc st + 1 + off }
exec (JMP_IF_FALSE off) st = case stack st of
    (v:r) -> case truthiness v of
        Right True -> Right st { stack = r, pc = pc st + 1 }
        Right False -> Right st { stack = r, pc = pc st + 1 + off }
        Left msg -> Left msg
    [] -> Left "JMP_IF_FALSE: empty stack"
exec (JMP_IF_TRUE off) st = case stack st of
    (v:r) -> case truthiness v of
        Right True -> Right st { stack = r, pc = pc st + 1 + off }
        Right False -> Right st { stack = r, pc = pc st + 1 }
        Left msg -> Left msg
    [] -> Left "JMP_IF_TRUE: empty stack"
exec (CALL argc) st = case stack st of
    (VClosure params body:r) ->
        if length params /= argc
        then Left $ "CALL: expected " ++ show (length params) ++ " args, got " ++ show argc
        else let (args, rest') = splitAt argc r
                 newEnv = foldr (uncurry Map.insert) (env st) (zip params (reverse args))
             in Right st { stack = rest', env = newEnv, code = body, pc = 0
                         , callStack = (pc st + 1, env st, code st) : callStack st }
    (VBuiltin name:r) -> execBuiltin name argc r st
    _ -> Left "CALL: not a function"
exec RET st = case callStack st of
    ((retPc, retEnv, retCode):rest) -> case stack st of
        (result:r) -> Right st { stack = result:r, env = retEnv, code = retCode
                                , pc = retPc, callStack = rest }
        [] -> Left "RET: empty stack"
    [] -> Right st { halted = True }
exec (MAKE_CLOSURE params body) st = Right st { stack = VClosure params body : stack st, pc = pc st + 1 }
exec PRINT st = case stack st of
    (VInt argc:r) -> let (args, rest') = splitAt argc r
                     in Right st { stack = VNil : rest', pc = pc st + 1
                                 , output = unwords (map showValue (reverse args)) : output st }
    _ -> Left "PRINT: expected argument count"
exec CONCAT st = case stack st of
    (VString b:VString a:r) -> Right st { stack = VString (a ++ b) : r, pc = pc st + 1 }
    (b:VString a:r) -> Right st { stack = VString (a ++ showValue b) : r, pc = pc st + 1 }
    (VString b:a:r) -> Right st { stack = VString (showValue a ++ b) : r, pc = pc st + 1 }
    _ -> Left "CONCAT: invalid operands"
exec (MAKE_LIST n) st = 
    let (elems, rest) = splitAt n (stack st)
    in Right st { stack = VList (reverse elems) : rest, pc = pc st + 1 }
exec LIST_GET st = case stack st of
    (VInt idx:VList vs:r) -> 
        if idx >= 0 && idx < length vs
        then Right st { stack = (vs !! idx) : r, pc = pc st + 1 }
        else Left $ "LIST_GET: index " ++ show idx ++ " out of bounds"
    _ -> Left "LIST_GET: expected list and integer"
exec LIST_LEN st = case stack st of
    (VList vs:r) -> Right st { stack = VInt (length vs) : r, pc = pc st + 1 }
    _ -> Left "LIST_LEN: expected list"
exec BREAK _ = Left "BREAK: break outside of loop"
exec CONTINUE _ = Left "CONTINUE: continue outside of loop"
exec NOP st = Right st { pc = pc st + 1 }
exec HALT st = Right st { halted = True }
exec (LINE n) st = Right st { currentLine = n, pc = pc st + 1 }

fact :: Int -> Int
fact n = go 1 2
    where
        go acc i
                | i > n = acc
                | otherwise = go (acc * i) (i + 1)

truthiness :: Value -> Either String Bool
truthiness (VBool b) = Right b
truthiness VNil = Right False
truthiness (VList xs) = Right (not (null xs))
truthiness _ = Left "JMP_IF_*: expected boolean or list"

intBinOp :: (Int -> Int -> Int) -> String -> VMState -> Either String VMState
intBinOp op name st = case stack st of
    (VInt b:VInt a:r) -> Right st { stack = VInt (op a b) : r, pc = pc st + 1 }
    _ -> Left $ name ++ ": invalid operands"

cmpOp :: (Int -> Int -> Bool) -> String -> VMState -> Either String VMState
cmpOp op name st = case stack st of
    (VInt b:VInt a:r) -> Right st { stack = VBool (op a b) : r, pc = pc st + 1 }
    _ -> Left $ name ++ ": invalid operands"

execBuiltin :: String -> Int -> [Value] -> VMState -> Either String VMState
execBuiltin "+" 2 (VInt b:VInt a:r) st = Right st { stack = VInt (a+b):r, pc = pc st + 1 }
execBuiltin "-" 2 (VInt b:VInt a:r) st = Right st { stack = VInt (a-b):r, pc = pc st + 1 }
execBuiltin "*" 2 (VInt b:VInt a:r) st = Right st { stack = VInt (a*b):r, pc = pc st + 1 }
execBuiltin "div" 2 (VInt 0:_:_) _ = Left "div: division by zero"
execBuiltin "div" 2 (VInt b:VInt a:r) st = Right st { stack = VInt (a `div` b):r, pc = pc st + 1 }
execBuiltin "mod" 2 (VInt 0:_:_) _ = Left "mod: division by zero"
execBuiltin "mod" 2 (VInt b:VInt a:r) st = Right st { stack = VInt (a `mod` b):r, pc = pc st + 1 }
execBuiltin "eq?" 2 (VInt b:VInt a:r) st = Right st { stack = VBool (a==b):r, pc = pc st + 1 }
execBuiltin "<" 2 (VInt b:VInt a:r) st = Right st { stack = VBool (a<b):r, pc = pc st + 1 }
execBuiltin ">" 2 (VInt b:VInt a:r) st = Right st { stack = VBool (a>b):r, pc = pc st + 1 }
execBuiltin "len" 1 (VList vs:r) st = Right st { stack = VInt (length vs):r, pc = pc st + 1 }
execBuiltin "len" 1 (VString s:r) st = Right st { stack = VInt (length s):r, pc = pc st + 1 }
execBuiltin "get" 2 (VInt idx:VList vs:r) st = 
    if idx >= 0 && idx < length vs
    then Right st { stack = (vs !! idx):r, pc = pc st + 1 }
    else Left $ "get: index " ++ show idx ++ " out of bounds"
execBuiltin "get" 2 (VInt idx:VString s:r) st = 
    if idx >= 0 && idx < length s
    then Right st { stack = VString [s !! idx]:r, pc = pc st + 1 }
    else Left $ "get: index " ++ show idx ++ " out of bounds"
execBuiltin "append" 2 (v:VList vs:r) st =
    Right st { stack = VList (vs ++ [v]) : r, pc = pc st + 1 }
execBuiltin "append" 2 (_:_:_) _ = Left "append: expected (list, value)"
execBuiltin "set" 3 (v:VInt idx:VList vs:r) st =
    if idx >= 0 && idx < length vs
    then Right st { stack = VList (take idx vs ++ [v] ++ drop (idx + 1) vs) : r, pc = pc st + 1 }
    else Left $ "set: index " ++ show idx ++ " out of bounds"
execBuiltin "set" 3 _ _ = Left "set: expected (list, int, value)"
execBuiltin "concat" 2 (VList bs:VList as:r) st =
    Right st { stack = VList (as ++ bs) : r, pc = pc st + 1 }
execBuiltin "concat" 2 _ _ = Left "concat: expected (list, list)"
execBuiltin name _ _ _ = Left $ "Unknown builtin: " ++ name
