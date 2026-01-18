{-- 
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- ErrorMessage.hs
--}

module ErrorMessage (
    Color(..), colorize, bold, dim, underline, resetColor,
    formatError, formatWarning, formatSuccess, formatInfo, formatHint,
    ErrorType(..), formatParseError, formatRuntimeError, formatTypeError,
    formatUndefinedError, formatDivisionError, formatArgumentError, formatSyntaxError,
    showCodeContext, formatLocation
) where

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | White | Gray
    deriving (Eq, Show)

data ErrorType = ParseError | RuntimeError | TypeError | UndefinedError 
               | DivisionError | ArgumentError | SyntaxError
    deriving (Eq, Show)

resetColor :: String
resetColor = "\ESC[0m"

colorCode :: Color -> String
colorCode Red = "\ESC[31m"
colorCode Green = "\ESC[32m"
colorCode Yellow = "\ESC[33m"
colorCode Blue = "\ESC[34m"
colorCode Magenta = "\ESC[35m"
colorCode Cyan = "\ESC[36m"
colorCode White = "\ESC[37m"
colorCode Gray = "\ESC[90m"

brightCode :: Color -> String
brightCode Red = "\ESC[91m"
brightCode Green = "\ESC[92m"
brightCode Yellow = "\ESC[93m"
brightCode Blue = "\ESC[94m"
brightCode Magenta = "\ESC[95m"
brightCode Cyan = "\ESC[96m"
brightCode White = "\ESC[97m"
brightCode Gray = "\ESC[90m"

colorize :: Color -> String -> String
colorize c t = colorCode c ++ t ++ resetColor

bright :: Color -> String -> String
bright c t = brightCode c ++ t ++ resetColor

bold :: String -> String
bold t = "\ESC[1m" ++ t ++ resetColor

dim :: String -> String
dim t = "\ESC[2m" ++ t ++ resetColor

underline :: String -> String
underline t = "\ESC[4m" ++ t ++ resetColor

formatError :: String -> String
formatError msg = bold (bright Red "error") ++ bold ": " ++ msg

formatWarning :: String -> String
formatWarning msg = bold (bright Yellow "warning") ++ bold ": " ++ msg

formatSuccess :: String -> String
formatSuccess msg = bold (bright Green "success") ++ bold ": " ++ msg

formatInfo :: String -> String
formatInfo msg = bold (bright Blue "info") ++ bold ": " ++ msg

formatHint :: String -> String
formatHint msg = colorize Cyan "  hint: " ++ msg

formatLocation :: Maybe FilePath -> Int -> Int -> String
formatLocation mPath line col = 
    colorize Cyan "  --> " ++ maybe "<stdin>" id mPath ++ ":" ++ show line ++ ":" ++ show col

showCodeContext :: String -> Int -> Int -> Int -> String
showCodeContext source line col len =
    let ls = lines source
        pad = length (show line) + 1
        numStr = replicate (pad - length (show line)) ' ' ++ show line
        emptyPad = replicate pad ' '
        srcLine = if line > 0 && line <= length ls then ls !! (line - 1) else ""
    in unlines
        [ colorize Blue (emptyPad ++ " |")
        , colorize Blue (numStr ++ " | ") ++ srcLine
        , colorize Blue (emptyPad ++ " | ") ++ replicate (col - 1) ' ' ++ bright Red (replicate (max 1 len) '^')
        ]

formatParseError :: Maybe FilePath -> String -> Int -> Int -> String -> String
formatParseError mPath source line col msg = unlines
    [ formatError msg
    , formatLocation mPath line col
    , showCodeContext source line col 1
    ]

formatRuntimeError :: String -> String -> String
formatRuntimeError ctx msg = unlines
    [ formatError msg
    , colorize Cyan "  in: " ++ ctx
    , ""
    ]

formatTypeError :: String -> String -> String -> String
formatTypeError expected got ctx = unlines
    [ formatError "type mismatch"
    , formatHint $ "expected " ++ bold expected ++ ", got " ++ bold got
    , colorize Cyan "  in: " ++ ctx
    , ""
    ]

formatUndefinedError :: String -> [String] -> String
formatUndefinedError name suggestions = unlines $
    [ formatError $ "undefined variable '" ++ bold name ++ "'"
    ] ++ case suggestions of
        (s:_) -> [formatHint $ "did you mean '" ++ bold s ++ "'?"]
        [] -> []

formatDivisionError :: String -> String
formatDivisionError ctx = unlines
    [ formatError "division by zero"
    , colorize Cyan "  in: " ++ ctx
    , formatHint "the divisor must be non-zero"
    , ""
    ]

formatArgumentError :: String -> Int -> Int -> String
formatArgumentError func expected got = unlines
    [ formatError "wrong number of arguments"
    , formatHint $ "function '" ++ bold func ++ "' expects " ++ 
                   bold (show expected) ++ " argument(s), got " ++ bold (show got)
    , ""
    ]

formatSyntaxError :: Maybe FilePath -> String -> Int -> Int -> String -> Maybe String -> String
formatSyntaxError mPath source line col msg mSugg = unlines $
    [ formatError msg
    , formatLocation mPath line col
    , showCodeContext source line col 1
    ] ++ maybe [] (\s -> [formatHint s]) mSugg
