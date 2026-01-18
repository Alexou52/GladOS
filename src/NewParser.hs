{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- NewParser.hs
--}

module NewParser (parseProgram, parseExpression) where

import AST
import Lexer

data ParseState = ParseState { tokens :: [Token], errors :: [String] }
    deriving (Show)

newtype Parser a = Parser { runParser :: ParseState -> Either String (a, ParseState) }

instance Functor Parser where
    fmap f p = Parser $ \s -> fmap (\(a, s') -> (f a, s')) (runParser p s)

instance Applicative Parser where
    pure a = Parser $ \s -> Right (a, s)
    pf <*> pa = Parser $ \s -> do
        (f, s') <- runParser pf s
        (a, s'') <- runParser pa s'
        return (f a, s'')

instance Monad Parser where
    pa >>= f = Parser $ \s -> runParser pa s >>= \(a, s') -> runParser (f a) s'

parseError :: String -> Parser a
parseError msg = Parser $ \s -> 
    let pos = case tokens s of
            (t:_) -> " at line " ++ show (tokenLine t) ++ ", col " ++ show (tokenCol t)
            [] -> " at end of input"
    in Left (msg ++ pos)

peek :: Parser (Maybe Token)
peek = Parser $ \s -> Right (case tokens s of { [] -> Nothing; (x:_) -> Just x }, s)

getCurrentPos :: Parser SourcePos
getCurrentPos = Parser $ \s -> Right (case tokens s of
    (t:_) -> SourcePos (tokenLine t) (tokenCol t)
    [] -> SourcePos 0 0, s)

located :: Parser Ast -> Parser Ast
located p = Located <$> getCurrentPos <*> p

advance :: Parser Token
advance = Parser $ \s -> case tokens s of
    [] -> Left "Unexpected end of input"
    (t:ts) -> Right (t, s { tokens = ts })

match :: TokenType -> Parser Bool
match expected = maybe False ((== expected) . tokenType) <$> peek

expect :: TokenType -> String -> Parser Token
expect expected msg = peek >>= \mt -> case mt of
    Just tok | tokenType tok == expected -> advance
    Just tok -> parseError $ msg ++ ", got " ++ show (tokenType tok)
    Nothing -> parseError $ msg ++ ", got end of input"

skipNewlines :: Parser ()
skipNewlines = match TokNewline >>= \b -> if b then advance >> skipNewlines else return ()

-- Parse an indented suite (Python-like) based on token column positions.
-- Requires that at least one newline has been consumed after the ':' token.
indentedSuite :: Int -> Parser Ast
indentedSuite baseCol = do
    mt <- peek
    case mt of
        Nothing -> parseError "Expected indented block"
        Just t -> do
            let suiteCol = tokenCol t
            if suiteCol <= baseCol
                then parseError "Expected indented block"
                else Block <$> manyIndentedStatementsAt suiteCol baseCol

manyIndentedStatementsAt :: Int -> Int -> Parser [Ast]
manyIndentedStatementsAt suiteCol baseCol = do
    skipNewlines
    mt <- peek
    case mt of
        Nothing -> return []
        Just t
            | tokenType t == TokEOF -> return []
            | tokenCol t <= baseCol -> return []
            | tokenCol t /= suiteCol -> parseError "Indentation error"
            | otherwise -> do
                stmt <- located statementBody
                skipNewlines
                rest <- manyIndentedStatementsAt suiteCol baseCol
                return (stmt : rest)

parseProgram :: String -> Either String [Ast]
parseProgram input = do
    toks <- tokenize input
    -- The lexer inserts TokIndent/TokDedent to represent indentation.
    -- This parser uses token column positions directly (Python-like), so
    -- keeping these synthetic tokens would break lookahead after ':' and
    -- inside suites.
    let st = ParseState { tokens = filter isSignificant toks, errors = [] }
    fst <$> runParser programParser st
  where
    isSignificant t = tokenType t `notElem` [TokIndent, TokDedent]

programParser :: Parser [Ast]
programParser = skipNewlines >> manyStatementsUntilEOF <* expect TokEOF "Expected end of file"

manyStatementsUntilEOF :: Parser [Ast]
manyStatementsUntilEOF = do
    skipNewlines
    isEnd <- match TokEOF
    if isEnd
        then return []
        else do
            stmt <- statement
            rest <- manyStatementsUntilEOF
            return (stmt : rest)

many :: Parser a -> Parser [a]
many p = go []
  where go acc = optional p >>= maybe (return $ reverse acc) (\x -> go (x:acc))

optional :: Parser a -> Parser (Maybe a)
optional p = Parser $ \s -> case runParser p s of
    Left _ -> Right (Nothing, s)
    Right (a, s') -> Right (Just a, s')

statement :: Parser Ast
statement = skipNewlines >> located statementBody <* skipNewlines

statementBody :: Parser Ast
statementBody = peek >>= \mt -> case mt of
    Just tok -> case tokenType tok of
        TokReturn -> returnStmt
        TokPrint -> printStmt
        TokBreak -> breakStmt
        TokContinue -> continueStmt
        TokLet -> variableDef
        TokDef -> functionDef
        _ -> expressionStmt
    Nothing -> parseError "Expected statement"

returnStmt :: Parser Ast
returnStmt = expect TokReturn "Expected 'return'" >> Return <$> expression

breakStmt :: Parser Ast
breakStmt = expect TokBreak "Expected 'break'" >> return Break

continueStmt :: Parser Ast
continueStmt = expect TokContinue "Expected 'continue'" >> return Continue

printStmt :: Parser Ast
printStmt = do
    _ <- expect TokPrint "Expected 'print'"
    _ <- expect TokLParen "Expected '('"
    args <- parsePrintArgs
    _ <- expect TokRParen "Expected ')'"
    return $ Print args

parsePrintArgs :: Parser [Ast]
parsePrintArgs = match TokRParen >>= \b -> if b then return [] else do
    first <- expression
    rest <- many (expect TokComma "Expected ','" >> expression)
    return $ first : rest

variableDef :: Parser Ast
variableDef = do
    _ <- expect TokLet "Expected 'let'"
    name <- tokenValue <$> expect TokIdent "Expected identifier"
    _ <- expect TokAssign "Expected '='"
    Define name <$> expression

functionDef :: Parser Ast
functionDef = do
    defTok <- expect TokDef "Expected 'def'"
    name <- tokenValue <$> expect TokIdent "Expected function name"
    _ <- expect TokLParen "Expected '('"
    params <- parseParams
    _ <- expect TokRParen "Expected ')'"
    colonTok <- expect TokColon "Expected ':'"
    skipNewlines
    mtAfter <- peek
    let onNewLine = maybe False (\t -> tokenLine t > tokenLine colonTok) mtAfter
    isBrace <- match TokLBrace
    body <- if isBrace then braceSuite
        else if onNewLine then indentedSuite (tokenCol defTok) else statementBody
    return $ Define name (Lambda params body)

braceSuite :: Parser Ast
braceSuite = do
    _ <- expect TokLBrace "Expected '{'"
    skipNewlines
    stmts <- manyBlockStatements
    skipNewlines
    _ <- expect TokRBrace "Expected '}'"
    return $ Block stmts

parseParams :: Parser [String]
parseParams = match TokIdent >>= \b -> if b then do
    first <- tokenValue <$> advance
    rest <- many (expect TokComma "Expected ','" >> tokenValue <$> expect TokIdent "Expected parameter")
    return $ first : rest
  else return []

expressionStmt :: Parser Ast
expressionStmt = do
    expr <- expression
    -- Check if this is an assignment (ident = expr)
    isAssign <- match TokAssign
    if isAssign
        then case expr of
            AstSymbol name -> advance >> Define name <$> expression
            Located _ (AstSymbol name) -> advance >> Define name <$> expression
            _ -> parseError "Invalid assignment target"
        else return expr

expression :: Parser Ast
expression = peek >>= \mt -> case mt of
    Just tok -> case tokenType tok of
        TokIf -> ifExpr
        TokFor -> forExpr
        TokWhile -> whileExpr
        TokFn -> lambdaExpr
        _ -> comparison
    Nothing -> comparison

forExpr :: Parser Ast
forExpr = do
    forTok <- expect TokFor "Expected 'for'"
    varName <- tokenValue <$> expect TokIdent "Expected variable name"
    _ <- expect TokIn "Expected 'in'"
    isRange <- match TokRange
    forKind <-
        if isRange
            then do
                _ <- expect TokRange "Expected 'range'"
                _ <- expect TokLParen "Expected '('"
                startExpr <- comparison
                _ <- expect TokComma "Expected ','"
                endExpr <- comparison
                _ <- expect TokRParen "Expected ')'"
                return (Left (startExpr, endExpr))
            else do
                it <- comparison
                return (Right it)
    colonTok <- expect TokColon "Expected ':'"
    skipNewlines
    mtAfter <- peek
    let onNewLine = maybe False (\t -> tokenLine t > tokenLine colonTok) mtAfter
    isBrace <- match TokLBrace
    body <- if isBrace then braceSuite
        else if onNewLine then indentedSuite (tokenCol forTok) else loopSingleStatement
    case forKind of
        Left (s, e) -> return $ For varName s e body
        Right it -> return $ ForEach varName it body

-- Parse multiple statements until we see }
manyBlockStatements :: Parser [Ast]
manyBlockStatements = do
    isEnd <- match TokRBrace
    if isEnd
        then return []
        else do
            stmt <- blockStatement
            rest <- manyBlockStatements
            return (stmt : rest)

-- A single statement inside a block
blockStatement :: Parser Ast
blockStatement = skipNewlines >> peek >>= \mt -> case mt of
    Just tok -> case tokenType tok of
        TokReturn -> located returnStmt <* skipNewlines
        TokPrint -> located printStmt <* skipNewlines
        TokBreak -> located breakStmt <* skipNewlines
        TokContinue -> located continueStmt <* skipNewlines
        TokLet -> located variableDef <* skipNewlines
        TokDef -> located functionDef <* skipNewlines
        TokIf -> located ifExpr <* skipNewlines
        TokFor -> located forExpr <* skipNewlines
        TokWhile -> located whileExpr <* skipNewlines
        TokRBrace -> parseError "Empty block not allowed"
        TokEOF -> parseError "Unexpected end of file in block"
        _ -> located blockExprOrAssign <* skipNewlines
    Nothing -> parseError "Unexpected end of input"

-- Parse either an expression or an assignment in block
blockExprOrAssign :: Parser Ast
blockExprOrAssign = do
    expr <- comparison
    isAssign <- match TokAssign
    if isAssign
        then case expr of
            AstSymbol name -> advance >> Define name <$> expression
            Located _ (AstSymbol name) -> advance >> Define name <$> expression
            _ -> parseError "Invalid assignment target"
        else return expr

-- Parse a single statement for loop body (one statement only)
loopSingleStatement :: Parser Ast
loopSingleStatement = peek >>= \mt -> case mt of
    Just tok -> case tokenType tok of
        TokReturn -> located returnStmt
        TokPrint -> located printStmt
        TokBreak -> located breakStmt
        TokContinue -> located continueStmt
        TokLet -> located variableDef
        TokIf -> located ifExpr
        TokFor -> located forExpr
        TokWhile -> located whileExpr
        _ -> located loopExprOrAssign
    Nothing -> parseError "Expected loop body"

-- Parse either an expression or an assignment in loop body
loopExprOrAssign :: Parser Ast
loopExprOrAssign = do
    expr <- comparison
    isAssign <- match TokAssign
    if isAssign
        then case expr of
            AstSymbol name -> advance >> Define name <$> expression
            Located _ (AstSymbol name) -> advance >> Define name <$> expression
            _ -> parseError "Invalid assignment target"
        else return expr

whileExpr :: Parser Ast
whileExpr = do
    whileTok <- expect TokWhile "Expected 'while'"
    cond <- comparison
    colonTok <- expect TokColon "Expected ':'"
    skipNewlines
    mtAfter <- peek
    let onNewLine = maybe False (\t -> tokenLine t > tokenLine colonTok) mtAfter
    isBrace <- match TokLBrace
    body <- if isBrace then braceSuite
        else if onNewLine then indentedSuite (tokenCol whileTok) else loopSingleStatement
    return $ While cond body

ifExpr :: Parser Ast
ifExpr = do
    ifTok <- expect TokIf "Expected 'if'"
    cond <- comparison
    colonThenTok <- expect TokColon "Expected ':'"
    skipNewlines
    mtThen <- peek
    let thenOnNewLine = maybe False (\t -> tokenLine t > tokenLine colonThenTok) mtThen
    isBraceThen <- match TokLBrace
    thenBr <- if isBraceThen then braceSuite
              else if thenOnNewLine then indentedSuite (tokenCol ifTok) else statementBody
    skipNewlines
    _ <- expect TokElse "Expected 'else'"
    colonElseTok <- expect TokColon "Expected ':'"
    skipNewlines
    mtElse <- peek
    let elseOnNewLine = maybe False (\t -> tokenLine t > tokenLine colonElseTok) mtElse
    isBraceElse <- match TokLBrace
    elseBr <- if isBraceElse then braceSuite
              else if elseOnNewLine then indentedSuite (tokenCol ifTok) else statementBody
    return $ If cond thenBr elseBr

lambdaExpr :: Parser Ast
lambdaExpr = do
    _ <- expect TokFn "Expected 'fn'"
    _ <- expect TokLParen "Expected '('"
    params <- parseParams
    _ <- expect TokRParen "Expected ')'"
    _ <- expect TokColon "Expected ':'"
    Lambda params <$> expression

comparison :: Parser Ast
comparison = additive >>= comparisonRest

comparisonRest :: Ast -> Parser Ast
comparisonRest left = peek >>= \mt -> case mt of
    Just tok | tokenType tok `elem` [TokEq, TokNeq, TokLt, TokGt, TokLte, TokGte] -> do
        _ <- advance
        right <- additive
        comparisonRest $ AstCall (AstSymbol $ compOp $ tokenType tok) [left, right]
    _ -> return left
  where
    compOp TokEq = "eq?"; compOp TokNeq = "neq?"; compOp TokLt = "<"
    compOp TokGt = ">"; compOp TokLte = "<="; compOp TokGte = ">="; compOp _ = "?"

additive :: Parser Ast
additive = multiplicative >>= additiveRest

additiveRest :: Ast -> Parser Ast
additiveRest left = peek >>= \mt -> case mt of
    Just tok | tokenType tok == TokPlus -> advance >> multiplicative >>= additiveRest . AstCall (AstSymbol "+") . ([left] ++) . (:[])
    Just tok | tokenType tok == TokMinus -> advance >> multiplicative >>= additiveRest . AstCall (AstSymbol "-") . ([left] ++) . (:[])
    _ -> return left

multiplicative :: Parser Ast
multiplicative = unary >>= multiplicativeRest

multiplicativeRest :: Ast -> Parser Ast
multiplicativeRest left = peek >>= \mt -> case mt of
    Just tok | tokenType tok == TokMult -> advance >> unary >>= multiplicativeRest . AstCall (AstSymbol "*") . ([left] ++) . (:[])
    Just tok | tokenType tok == TokDiv -> advance >> unary >>= multiplicativeRest . AstCall (AstSymbol "div") . ([left] ++) . (:[])
    Just tok | tokenType tok == TokMod -> advance >> unary >>= multiplicativeRest . AstCall (AstSymbol "mod") . ([left] ++) . (:[])
    _ -> return left

unary :: Parser Ast
unary = peek >>= \mt -> case mt of
    Just tok | tokenType tok == TokMinus -> advance >> AstCall (AstSymbol "-") . (AstInt 0 :) . (:[]) <$> unary
    Just tok | tokenType tok == TokNot -> advance >> AstCall (AstSymbol "not") . (:[]) <$> unary
    _ -> power

-- Exponentiation (**), right-associative.
-- Example: 2 ** 3 ** 2 parses as 2 ** (3 ** 2)
power :: Parser Ast
power = postfix >>= powerRest

powerRest :: Ast -> Parser Ast
powerRest left = match TokPow >>= \b -> if b
    then do
        _ <- advance
        right <- power
        return $ AstCall (AstSymbol "**") [left, right]
    else return left

-- Postfix operators (currently factorial: expr!)
postfix :: Parser Ast
postfix = call >>= postfixRest

postfixRest :: Ast -> Parser Ast
postfixRest expr = match TokNot >>= \b -> if b
    then do
        _ <- advance
        postfixRest $ AstCall (AstSymbol "!") [expr]
    else return expr

call :: Parser Ast
call = primary >>= callRest

callRest :: Ast -> Parser Ast
callRest callee = do
    isCall <- match TokLParen
    isIndex <- match TokLBracket
    if isCall
        then do
            _ <- advance
            args <- parseArgs
            _ <- expect TokRParen "Expected ')'"
            callRest (AstCall callee args)
        else if isIndex
            then do
                _ <- advance
                idx <- expression
                _ <- expect TokRBracket "Expected ']'"
                callRest (AstCall (AstSymbol "get") [callee, idx])
            else return callee

parseArgs :: Parser [Ast]
parseArgs = match TokRParen >>= \b -> if b then return [] else do
    first <- expression
    rest <- many (expect TokComma "Expected ','" >> expression)
    return $ first : rest

primary :: Parser Ast
primary = peek >>= \mt -> case mt of
    Just tok -> case tokenType tok of
        TokInt -> advance >> return (AstInt $ read $ tokenValue tok)
        TokBool -> advance >> return (AstBool $ tokenValue tok == "true")
        TokString -> advance >> return (AstString $ tokenValue tok)
        TokIdent -> advance >> return (AstSymbol $ tokenValue tok)
        TokLParen -> advance >> expression <* expect TokRParen "Expected ')'"
        TokLBracket -> listLiteral
        _ -> parseError $ "Unexpected token: " ++ show (tokenType tok)
    Nothing -> parseError "Unexpected end of input"

listLiteral :: Parser Ast
listLiteral = do
    _ <- expect TokLBracket "Expected '['"
    isEmpty <- match TokRBracket
    if isEmpty
        then expect TokRBracket "Expected ']'" >> return (AstList [])
        else do
            first <- expression
            rest <- listLiteralRest
            _ <- expect TokRBracket "Expected ']'"
            return $ AstList (first : rest)

listLiteralRest :: Parser [Ast]
listLiteralRest = do
    hasComma <- match TokComma
    if hasComma
        then do
            _ <- advance
            isEnd <- match TokRBracket
            if isEnd
                then return []
                else do
                    x <- expression
                    xs <- listLiteralRest
                    return (x : xs)
        else return []

parseExpression :: String -> Either String Ast
parseExpression input = tokenize input >>= \toks -> 
    fst <$> runParser expression (ParseState toks [])
