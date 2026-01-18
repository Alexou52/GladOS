{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Reader.hs
--}

module Reader (parseManySExpr, parseSExpr) where

import SExpr

parseManySExpr :: String -> Either String [SExpr]
parseManySExpr input = fst <$> parseSeq (tokenize $ removeComments input)

parseSExpr :: String -> Either String SExpr
parseSExpr s = parseManySExpr s >>= \exprs -> case exprs of
    [one] -> Right one
    [] -> Left "empty input"
    _ -> Left "multiple expressions provided"

removeComments :: String -> String
removeComments = unlines . map (takeWhile (/= ';')) . lines

tokenize :: String -> [String]
tokenize = words . concatMap expand
  where
    expand '(' = " ( "
    expand ')' = " ) "
    expand c = [c]

parseSeq :: [String] -> Either String ([SExpr], [String])
parseSeq [] = Right ([], [])
parseSeq toks = go [] toks
  where
    go acc [] = Right (reverse acc, [])
    go acc ts = parseOne ts >>= \(e, rest) -> go (e:acc) rest

parseOne :: [String] -> Either String (SExpr, [String])
parseOne [] = Left "unexpected end of tokens"
parseOne ("(":rest) = parseList rest >>= \(xs, after) -> Right (SList xs, after)
parseOne (")":_) = Left "unexpected ')'"
parseOne (atom:rest) = Right (toAtom atom, rest)

parseList :: [String] -> Either String ([SExpr], [String])
parseList [] = Left "unterminated list"
parseList (")":rest) = Right ([], rest)
parseList toks = do
    (h, afterH) <- parseOne toks
    (t, afterT) <- parseList afterH
    return (h:t, afterT)

toAtom :: String -> SExpr
toAtom s = case reads s of
    [(n, "")] -> SInt n
    _ -> SSymbol s
