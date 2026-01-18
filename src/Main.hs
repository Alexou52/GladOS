{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Main.hs
--}

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, IOException)
import Data.List (isSuffixOf, isInfixOf)

import Reader (parseManySExpr)
import Parser (sexprToAST)
import Eval (evalASTWithEnv, Env, initialEnv)
import SExpr (SExpr)
import AST (Ast(..))
import NewParser (parseProgram)
import Compiler (compile)
import VM (runProgramWithArgs, VMOutput(..))
import Bytecode (Value(..), showValue, Program)
import BytecodeFile (compileToBinaryFile, loadFromBinaryFile)
import ErrorMessage

data Mode = LispMode | NewMode | CompiledMode deriving (Eq, Show)

main :: IO ()
main = getArgs >>= \args -> case args of
    [] -> runLispStdin
    ["-h"] -> printHelp
    ["--help"] -> printHelp
    ["-b", f] -> compileToBinary f
    ["--build", f] -> compileToBinary f
    ["-c", f] -> runCompiled f []
    ["--compile", f] -> runCompiled f []
    (f:rest) -> runFile f rest

printHelp :: IO ()
printHelp = do
    putStrLn $ bold "GLaDOS" ++ " - A Python-like Language Interpreter"
    putStrLn ""
    putStrLn $ colorize Cyan "USAGE:"
    putStrLn "  glados                  Read LISP from stdin"
    putStrLn "  glados <file.scm>       Run LISP file"
    putStrLn "  glados <file.gla>       Run GLaDOS file"
    putStrLn "  glados <file.glc>       Run compiled bytecode"
    putStrLn "  glados -b <file.gla>    Compile to .glc"
    putStrLn "  glados -c <file>        Compile and run"
    putStrLn ""
    putStrLn $ colorize Cyan "FILE EXTENSIONS:"
    putStrLn "  .scm, .lisp  - LISP syntax"
    putStrLn "  .gla         - GLaDOS syntax"
    putStrLn "  .glc         - Compiled bytecode"

detectMode :: FilePath -> Mode
detectMode f
    | ".glc" `isSuffixOf` f = CompiledMode
    | ".gla" `isSuffixOf` f = NewMode
    | otherwise = LispMode

runFile :: FilePath -> [String] -> IO ()
runFile f programArgs = case detectMode f of
    CompiledMode -> runCompiledBytecode f programArgs
    LispMode -> readFile f `catch` handleFileError f >>= runLispContent
    NewMode -> readFile f `catch` handleFileError f >>= \c -> runNewContent f c programArgs

compileToBinary :: FilePath -> IO ()
compileToBinary f = do
    content <- readFile f `catch` handleFileError f
    case parseProgram content of
        Left err -> hPutStrLn stderr (formatParseErrorNew f content err) >> exitWith (ExitFailure 84)
        Right asts -> do
            compileToBinaryFile (replaceExt f ".glc") (compile asts)
            putStrLn $ colorize Green "✓ " ++ "Compilation successful!"

replaceExt :: FilePath -> String -> FilePath
replaceExt p new = let base = reverse $ drop 1 $ dropWhile (/= '.') $ reverse p
    in if null base then p ++ new else base ++ new

runCompiledBytecode :: FilePath -> [String] -> IO ()
runCompiledBytecode f programArgs = loadFromBinaryFile f >>= \res -> case res of
    Left err -> hPutStrLn stderr (formatRuntimeErrorNew f err) >> exitWith (ExitFailure 84)
    Right bc -> execBytecode f bc programArgs

runNewContent :: FilePath -> String -> [String] -> IO ()
runNewContent f c programArgs = case parseProgram c of
    Left err -> hPutStrLn stderr (formatParseErrorNew f c err) >> exitWith (ExitFailure 84)
    Right asts -> execBytecode f (compile asts) programArgs

execBytecode :: FilePath -> Program -> [String] -> IO ()
execBytecode f bc programArgs = case runProgramWithArgs (f:programArgs) bc of
    Left err -> hPutStrLn stderr (formatRuntimeErrorNew f err) >> exitWith (ExitFailure 84)
    Right out -> do
        mapM_ putStrLn (outputLines out)
        case outputValue out of
            VNil -> exitWith ExitSuccess
            val -> putStrLn (showValue val) >> exitWith ExitSuccess

formatParseErrorNew :: FilePath -> String -> String -> String
formatParseErrorNew f src err =
    let (line, col, msg) = parseErrorInfo err
    in formatSyntaxError (Just f) src line col msg (suggestFix msg)

parseErrorInfo :: String -> (Int, Int, String)
parseErrorInfo err = case extractLineCol err of
    Just (l, c) -> (l, c, cleanMsg err)
    Nothing -> (1, 1, err)

extractLineCol :: String -> Maybe (Int, Int)
extractLineCol msg = go $ words msg
  where
    go ("line":n:",":_) = Just (read (init n), 1)
    go ("line":n:"col":c:_) = Just (read (init n), read c)
    go ("at":"line":n:",":_) = Just (read (init n), 1)
    go ("at":"line":n:"col":c:_) = Just (read (init n), read c)
    go (_:xs) = go xs
    go [] = Nothing

cleanMsg :: String -> String
cleanMsg = unwords . go . words
  where go ("at":"line":_) = []; go (x:xs) = x : go xs; go [] = []

suggestFix :: String -> Maybe String
suggestFix msg
    | "Expected '('" `isInfixOf` msg = Just "function calls require parentheses"
    | "Expected ')'" `isInfixOf` msg = Just "check for missing closing parenthesis"
    | "Expected ':'" `isInfixOf` msg = Just "if/def statements need a colon"
    | "Expected 'else'" `isInfixOf` msg = Just "if statements require an else branch"
    | "Unterminated string" `isInfixOf` msg = Just "close the string with \""
    | otherwise = Nothing

formatRuntimeErrorNew :: FilePath -> String -> String
formatRuntimeErrorNew f err =
    let (lineNum, cleanErr) = extractLineFromError err
        loc = maybe f (\n -> f ++ ":" ++ show n) lineNum
    in unlines [formatError (extractRuntimeError cleanErr), colorize Cyan ("  --> " ++ loc), "", hintForRuntime cleanErr]

extractLineFromError :: String -> (Maybe Int, String)
extractLineFromError err
    | take 5 err == "line " = let (n, rest) = span (`elem` ['0'..'9']) (drop 5 err)
        in case (reads n :: [(Int, String)], rest) of
            ([(num, _)], ':':' ':msg) -> (Just num, msg)
            ([(num, _)], ':':msg) -> (Just num, msg)
            _ -> (Nothing, err)
    | otherwise = (Nothing, err)

extractRuntimeError :: String -> String
extractRuntimeError err
    | "division by zero" `isInfixOf` err = "division by zero"
    | "undefined variable" `isInfixOf` err = let s = dropWhile (/= '\'') err
        in if null s then err else "undefined variable " ++ takeWhile (/= '\'') (tail s) ++ "'"
    | "not a function" `isInfixOf` err = "attempted to call a non-function"
    | otherwise = err

hintForRuntime :: String -> String
hintForRuntime err
    | "division by zero" `isInfixOf` err = formatHint "divisor must be non-zero"
    | "undefined variable" `isInfixOf` err = formatHint "make sure the variable is defined with 'let'"
    | "LOAD:" `isInfixOf` err = formatHint "variable not found"
    | "CALL:" `isInfixOf` err = formatHint "check function arguments"
    | otherwise = ""

runLispStdin :: IO ()
runLispStdin = getContents >>= runLispContent

runLispContent :: String -> IO ()
runLispContent c = case parseManySExpr c of
    Left err -> hPutStrLn stderr (formatError $ "parse error: " ++ err) >> exitWith (ExitFailure 84)
    Right sexprs -> evalAllLisp sexprs initialEnv >>= \res -> case res of
        Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 84)
        Right _ -> exitWith ExitSuccess

runCompiled :: FilePath -> [String] -> IO ()
runCompiled f programArgs = readFile f `catch` handleFileError f >>= \c -> case detectMode f of
    NewMode -> runNewContent f c programArgs
    LispMode -> runLispCompiled f c
    CompiledMode -> runCompiledBytecode f programArgs

runLispCompiled :: FilePath -> String -> IO ()
runLispCompiled f c = case parseManySExpr c of
    Left err -> hPutStrLn stderr (formatError $ "parse error: " ++ err) >> exitWith (ExitFailure 84)
    Right sexprs -> case mapM sexprToAST sexprs of
        Nothing -> hPutStrLn stderr (formatError "invalid syntax") >> exitWith (ExitFailure 84)
        Just asts -> execBytecode f (compile asts) []

evalAllLisp :: [SExpr] -> Env -> IO (Either String ())
evalAllLisp [] _ = return $ Right ()
evalAllLisp (sx:xs) env = case sexprToAST sx of
    Nothing -> return $ Left (formatError $ "invalid syntax: " ++ show sx)
    Just ast -> let (res, newEnv) = evalASTWithEnv env ast in case res of
        Nothing -> return $ Left (makeErrorMsg ast env)
        Just v -> (case ast of Define {} -> return (); _ -> putStrLn $ show v) >> evalAllLisp xs newEnv

makeErrorMsg :: Ast -> Env -> String
makeErrorMsg (AstSymbol n) _ = formatError $ "variable '" ++ bold n ++ "' is not bound"
makeErrorMsg (AstCall func args) env = findUnboundCall func args env
makeErrorMsg ast _ = formatError $ "evaluation failed for: " ++ show ast

findUnboundCall :: Ast -> [Ast] -> Env -> String
findUnboundCall func args env = case func of
    AstSymbol n | n `elem` ["div", "mod"] -> formatError "division by zero" ++ "\n" ++ formatHint "divisor must be non-zero"
    _ -> case findUnbound (func:args) env of
        Just var -> formatError $ "variable '" ++ bold var ++ "' is not bound"
        Nothing -> formatError "evaluation failed"

findUnbound :: [Ast] -> Env -> Maybe String
findUnbound [] _ = Nothing
findUnbound (AstSymbol n:xs) env = case evalASTWithEnv env (AstSymbol n) of
    (Nothing, _) -> Just n
    _ -> findUnbound xs env
findUnbound (AstCall f as:xs) env = case findUnbound (f:as) env of
    r@(Just _) -> r
    Nothing -> findUnbound xs env
findUnbound (_:xs) env = findUnbound xs env

handleFileError :: FilePath -> IOException -> IO String
handleFileError f _ = do
    hPutStrLn stderr $ formatError $ "cannot read file: " ++ bold f
    hPutStrLn stderr $ formatHint "check that the file exists"
    exitWith (ExitFailure 84)
