{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Eval.hs
--}

module Eval where

import AST
import qualified Data.Map as Map
import Data.Maybe (maybeToList)

type Env = Map.Map String Ast

initialEnv :: Env
initialEnv = Map.fromList
    [ ("+", AstSymbol "+"), ("-", AstSymbol "-")
    , ("*", AstSymbol "*"), ("div", AstSymbol "div"), ("mod", AstSymbol "mod")
    , ("eq?", AstSymbol "eq?"), ("<", AstSymbol "<"), (">", AstSymbol ">")
    ]

evalAST :: Ast -> (Maybe Ast, Env)
evalAST = evalASTWithEnv initialEnv

evalASTWithEnv :: Env -> Ast -> (Maybe Ast, Env)
evalASTWithEnv env (AstInt n) = (Just $ AstInt n, env)
evalASTWithEnv env (AstBool b) = (Just $ AstBool b, env)
evalASTWithEnv env (AstSymbol name) = (Map.lookup name env, env)
evalASTWithEnv env lambda@(Lambda _ _) = (Just lambda, env)
evalASTWithEnv env (Define name value) =
    case evalASTWithEnv env value of
        (Just v, env1) -> (Just v, Map.insert name v env1)
        (Nothing, env1) -> (Nothing, env1)
evalASTWithEnv env (If cond thenE elseE) =
    case evalASTWithEnv env cond of
        (Just (AstBool True), env1) -> evalASTWithEnv env1 thenE
        (Just (AstBool False), env1) -> evalASTWithEnv env1 elseE
        _ -> (Nothing, env)
evalASTWithEnv env (AstCall func args) =
    case evalASTWithEnv env func of
        (Just f, env1) ->
            let (evaledArgs, env2) = evalArgs env1 args
            in if length evaledArgs == length args
               then (applyFunc env2 f evaledArgs, env2)
               else (Nothing, env2)
        _ -> (Nothing, env)
evalASTWithEnv env _ = (Nothing, env)

evalArgs :: Env -> [Ast] -> ([Ast], Env)
evalArgs env [] = ([], env)
evalArgs env (x:xs) =
    let (val, env1) = evalASTWithEnv env x
        (rest, env2) = evalArgs env1 xs
    in (maybeToList val ++ rest, env2)

applyFunc :: Env -> Ast -> [Ast] -> Maybe Ast
applyFunc env (Lambda params body) args
    | length params == length args =
        let localEnv = Map.union (Map.fromList $ zip params args) env
        in fst $ evalASTWithEnv localEnv body
    | otherwise = Nothing
applyFunc _ (AstSymbol "+") args = arith (+) args
applyFunc _ (AstSymbol "-") args = arith (-) args
applyFunc _ (AstSymbol "*") args = arith (*) args
applyFunc _ (AstSymbol "div") [AstInt a, AstInt b] = if b == 0 then Nothing else Just $ AstInt (a `div` b)
applyFunc _ (AstSymbol "mod") [AstInt a, AstInt b] = if b == 0 then Nothing else Just $ AstInt (a `mod` b)
applyFunc _ (AstSymbol "eq?") [AstInt a, AstInt b] = Just $ AstBool (a == b)
applyFunc _ (AstSymbol "eq?") [AstBool a, AstBool b] = Just $ AstBool (a == b)
applyFunc _ (AstSymbol "<") [AstInt a, AstInt b] = Just $ AstBool (a < b)
applyFunc _ (AstSymbol ">") [AstInt a, AstInt b] = Just $ AstBool (a > b)
applyFunc _ _ _ = Nothing

arith :: (Int -> Int -> Int) -> [Ast] -> Maybe Ast
arith op args = do
    ints <- mapM extractInt args
    case ints of
        [] -> Nothing
        [_] -> Nothing
        xs -> Just $ AstInt (foldl1 op xs)
  where
    extractInt (AstInt n) = Just n
    extractInt _ = Nothing
