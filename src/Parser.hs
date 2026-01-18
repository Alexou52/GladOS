{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Parser.hs
--}

module Parser where

import SExpr
import AST

sexprToAST :: SExpr -> Maybe Ast
sexprToAST = parseExpr

parseExpr :: SExpr -> Maybe Ast
parseExpr (SInt n) = Just $ AstInt n
parseExpr (SSymbol s) 
    | s `elem` ["#t", "true"] = Just $ AstBool True
    | s `elem` ["#f", "false"] = Just $ AstBool False
    | otherwise = Just $ AstSymbol s
parseExpr (SList []) = Nothing
parseExpr (SList [SSymbol "define", SSymbol name, value]) = 
    Define name <$> parseExpr value
parseExpr (SList [SSymbol "define", SList (SSymbol name : params), body]) = do
    paramNames <- mapM getSymbol params
    parsedBody <- parseExpr body
    return $ Define name (Lambda paramNames parsedBody)
parseExpr (SList (SSymbol "define" : _)) = Nothing
parseExpr (SList [SSymbol "if", cond, thenE, elseE]) = 
    If <$> parseExpr cond <*> parseExpr thenE <*> parseExpr elseE
parseExpr (SList (SSymbol "if" : _)) = Nothing
parseExpr (SList [SSymbol "lambda", SList params, body]) = 
    Lambda <$> mapM getSymbol params <*> parseExpr body
parseExpr (SList (SSymbol "lambda" : _)) = Nothing
parseExpr (SList (func:args)) = 
    AstCall <$> parseExpr func <*> mapM parseExpr args
