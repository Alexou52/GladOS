{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Lexer.hs
--}

module Lexer (Token(..), TokenType(..), tokenize, showToken) where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

data Token = Token
    { tokenType :: TokenType
    , tokenValue :: String
    , tokenLine :: Int
    , tokenCol :: Int
    } deriving (Eq)

instance Show Token where
    show = showToken

showToken :: Token -> String
showToken (Token t v l c) = show t ++ "(" ++ v ++ ") at " ++ show l ++ ":" ++ show c

data TokenType
    = TokInt | TokBool | TokString | TokIdent
    | TokLet | TokDef | TokFn | TokIf | TokElse | TokElif | TokReturn | TokPrint
    | TokFor | TokWhile | TokIn | TokRange | TokBreak | TokContinue
    | TokPlus | TokMinus | TokMult | TokDiv | TokMod | TokPow
    | TokEq | TokNeq | TokLt | TokGt | TokLte | TokGte
    | TokNot | TokAnd | TokOr
    | TokAssign | TokColon | TokComma
    | TokLParen | TokRParen | TokLBracket | TokRBracket | TokLBrace | TokRBrace
    | TokNewline | TokIndent | TokDedent | TokEOF
    deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize input = tokenizeRaw input 1 1 >>= processIndentation

tokenizeRaw :: String -> Int -> Int -> Either String [Token]
tokenizeRaw [] line col = Right [Token TokEOF "" line col]
tokenizeRaw ('#':rest) line _ = 
    tokenizeRaw (dropWhile (/= '\n') rest) line 1
tokenizeRaw ('\n':rest) line _ = 
    (Token TokNewline "\n" line 1 :) <$> tokenizeRaw rest (line + 1) 1
tokenizeRaw input@('"':_) line col = tokenizeString input line col
tokenizeRaw (c:rest) line col
    | isSpace c && c /= '\n' = tokenizeRaw rest line (col + 1)
    | isDigit c || (c == '-' && not (null rest) && isDigit (head rest)) = 
        tokenizeNumber (c:rest) line col
    | isAlpha c || c == '_' = tokenizeIdent (c:rest) line col
    | otherwise = tokenizeOp (c:rest) line col

tokenizeString :: String -> Int -> Int -> Either String [Token]
tokenizeString ('"':rest) line col = do
    let (str, after) = spanStr rest
    case after of
        ('"':remaining) -> (Token TokString str line col :) <$> tokenizeRaw remaining line (col + length str + 2)
        _ -> Left $ "Unterminated string at line " ++ show line
  where
    spanStr [] = ([], [])
    spanStr ('\\':'"':xs) = let (s, r) = spanStr xs in ('"':s, r)
    spanStr ('\\':'n':xs) = let (s, r) = spanStr xs in ('\n':s, r)
    spanStr ('\\':'t':xs) = let (s, r) = spanStr xs in ('\t':s, r)
    spanStr ('\\':'\\':xs) = let (s, r) = spanStr xs in ('\\':s, r)
    spanStr ('"':xs) = ([], '"':xs)
    spanStr (x:xs) = let (s, r) = spanStr xs in (x:s, r)
tokenizeString _ line col = Left $ "Invalid string at line " ++ show line ++ ":" ++ show col

tokenizeNumber :: String -> Int -> Int -> Either String [Token]
tokenizeNumber input line col =
    let (num, rest) = spanNum input
    in (Token TokInt num line col :) <$> tokenizeRaw rest line (col + length num)
  where
    spanNum ('-':xs) = let (n, r) = span isDigit xs in ('-':n, r)
    spanNum xs = span isDigit xs

tokenizeIdent :: String -> Int -> Int -> Either String [Token]
tokenizeIdent input line col =
    let (ident, rest) = span isIdentChar input
        tokType = identToTok ident
    in (Token tokType ident line col :) <$> tokenizeRaw rest line (col + length ident)
  where
    isIdentChar c = isAlphaNum c || c `elem` "_?!"

identToTok :: String -> TokenType
identToTok "let" = TokLet
identToTok "def" = TokDef
identToTok "fn" = TokFn
identToTok "if" = TokIf
identToTok "else" = TokElse
identToTok "elif" = TokElif
identToTok "return" = TokReturn
identToTok "print" = TokPrint
identToTok "for" = TokFor
identToTok "while" = TokWhile
identToTok "in" = TokIn
identToTok "range" = TokRange
identToTok "break" = TokBreak
identToTok "continue" = TokContinue
identToTok "true" = TokBool
identToTok "false" = TokBool
identToTok "and" = TokAnd
identToTok "or" = TokOr
identToTok _ = TokIdent

tokenizeOp :: String -> Int -> Int -> Either String [Token]
tokenizeOp input line col = case input of
    ('=':'=':r) -> cont TokEq "==" r 2
    ('!':'=':r) -> cont TokNeq "!=" r 2
    ('<':'=':r) -> cont TokLte "<=" r 2
    ('>':'=':r) -> cont TokGte ">=" r 2
    ('*':'*':r) -> cont TokPow "**" r 2
    ('+':r) -> cont TokPlus "+" r 1
    ('-':r) -> cont TokMinus "-" r 1
    ('*':r) -> cont TokMult "*" r 1
    ('/':r) -> cont TokDiv "/" r 1
    ('%':r) -> cont TokMod "%" r 1
    ('<':r) -> cont TokLt "<" r 1
    ('>':r) -> cont TokGt ">" r 1
    ('!':r) -> cont TokNot "!" r 1
    ('=':r) -> cont TokAssign "=" r 1
    (':':r) -> cont TokColon ":" r 1
    (',':r) -> cont TokComma "," r 1
    ('(':r) -> cont TokLParen "(" r 1
    (')':r) -> cont TokRParen ")" r 1
    ('[':r) -> cont TokLBracket "[" r 1
    (']':r) -> cont TokRBracket "]" r 1
    ('{':r) -> cont TokLBrace "{" r 1
    ('}':r) -> cont TokRBrace "}" r 1
    (c:_) -> Left $ "Unexpected '" ++ [c] ++ "' at line " ++ show line ++ ":" ++ show col
    [] -> Right [Token TokEOF "" line col]
  where
    cont t v r len = (Token t v line col :) <$> tokenizeRaw r line (col + len)

processIndentation :: [Token] -> Either String [Token]
processIndentation = go [0]
  where
    indentTok = Token TokIndent "" 0 0
    dedentTok = Token TokDedent "" 0 0

    go :: [Int] -> [Token] -> Either String [Token]
    go stk [] = Right $ replicate (length stk - 1) dedentTok
    go stk (tok:rest) = case tokenType tok of
        TokEOF -> Right $ replicate (length stk - 1) dedentTok ++ [tok]
        TokNewline -> do
            (ins, stk') <- adjustAfterNewline stk rest
            next <- go stk' rest
            Right $ tok : ins ++ next
        _ -> (tok :) <$> go stk rest

    adjustAfterNewline :: [Int] -> [Token] -> Either String ([Token], [Int])
    adjustAfterNewline stk ts = case dropWhile (\t -> tokenType t == TokNewline) ts of
        [] -> Right ([], stk)
        (t:_) | tokenType t == TokEOF -> Right ([], stk)
        (t:_) ->
            let spaces = max 0 (tokenCol t - 1)
                cur = head stk
            in if spaces > cur
                then Right ([indentTok], spaces : stk)
                else if spaces < cur
                    then do
                        (dedents, stk') <- popUntil spaces stk
                        Right (dedents, stk')
                    else Right ([], stk)

    popUntil :: Int -> [Int] -> Either String ([Token], [Int])
    popUntil target (cur:rest)
        | cur == target = Right ([], cur:rest)
        | cur > target = do
            (more, stk') <- popUntil target rest
            Right (dedentTok : more, stk')
        | otherwise = Left "Indentation error"
    popUntil _ [] = Left "Indentation error"
