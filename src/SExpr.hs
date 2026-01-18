{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- SExpr.hs
--}

module SExpr where

data SExpr 
    = SInt Int
    | SSymbol String
    | SList [SExpr]
    deriving (Show, Eq)

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInt n) = Just n
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList xs) = Just xs
getList _ = Nothing

printTree :: SExpr -> Maybe String
printTree (SInt n) = Just $ "a Number " ++ show n
printTree (SSymbol s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (SList []) = Just "an empty List"
printTree (SList xs) = Just $ "a List with " ++ go xs
  where
    go [] = ""
    go [x] = maybe "" id (printTree x)
    go (x:rest) = maybe "" (++ " followed by " ++ go rest) (printTree x)
