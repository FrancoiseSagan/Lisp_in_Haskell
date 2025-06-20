module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of 
        Left err -> "Error: " ++ show err
        Right output -> "Found"

main:: IO ()
main = getArgs >>= \(expr:_) -> putStrLn $ readExpr expr

