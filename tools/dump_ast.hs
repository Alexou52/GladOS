import System.Environment (getArgs)
import NewParser (parseProgram)

main :: IO ()
main = do
  [path] <- getArgs
  src <- readFile path
  case parseProgram src of
    Left err -> putStrLn err
    Right asts -> mapM_ print asts
