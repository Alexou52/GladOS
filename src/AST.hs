{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- AST.hs
--}

module AST where

data SourcePos = SourcePos { posLine :: Int, posCol :: Int }
    deriving (Eq, Show)

data Ast 
    = Define { defName :: String, defValue :: Ast }
    | AstInt { intValue :: Int }
    | AstString { stringValue :: String }
    | AstSymbol { symbolName :: String }
    | AstBool { boolValue :: Bool }
    | AstList { listElements :: [Ast] }
    | AstCall { callFunc :: Ast, callArgs :: [Ast] }
    | Lambda { lambdaParams :: [String], lambdaBody :: Ast }
    | If { ifCondition :: Ast, ifThen :: Ast, ifElse :: Ast }
    | For { forVar :: String, forStart :: Ast, forEnd :: Ast, forBody :: Ast }
    | ForEach String Ast Ast
    | While { whileCond :: Ast, whileBody :: Ast }
    | Break
    | Continue
    | Return { returnValue :: Ast }
    | Block { blockStmts :: [Ast] }
    | Print { printArgs :: [Ast] }
    | Located { locPos :: SourcePos, locAst :: Ast }
    deriving (Eq)

instance Show Ast where
    show (AstInt n) = show n
    show (AstString s) = "\"" ++ s ++ "\""
    show (AstBool True) = "true"
    show (AstBool False) = "false"
    show (AstList elems) = "[" ++ joinComma (map show elems) ++ "]"
    show (Lambda params _) = "<function(" ++ unwords params ++ ")>"
    show (AstSymbol s) = s
    show (Return v) = "return " ++ show v
    show (Print args) = "print(" ++ unwords (map show args) ++ ")"
    show (Block stmts) = "{ " ++ unwords (map show stmts) ++ " }"
    show (Located _ ast) = show ast
    show (Define n v) = "let " ++ n ++ " = " ++ show v
    show (AstCall f args) = show f ++ "(" ++ joinComma (map show args) ++ ")"
    show (If c t e) = "if " ++ show c ++ ": " ++ show t ++ " else: " ++ show e
    show (For v s e b) = "for " ++ v ++ " in range(" ++ show s ++ ", " ++ show e ++ "): " ++ show b
    show (ForEach v it b) = "for " ++ v ++ " in " ++ show it ++ ": " ++ show b
    show (While c b) = "while " ++ show c ++ ": " ++ show b
    show Break = "break"
    show Continue = "continue"

joinComma :: [String] -> String
joinComma [] = ""
joinComma [x] = x
joinComma (x:xs) = x ++ ", " ++ joinComma xs
