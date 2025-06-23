module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedQuote <|> noneOf "\""
                char '"'
                return $ String x
            where
                escapedQuote :: Parser char
                escapedQuote = do
                    char '\\'
                    escape <- oneOf "nrt\"\\"
                    case escape of 
                        'n' -> return '\n'
                        'r' -> return '\r'
                        't' -> return '\t'
                        '"' -> return '"'
                        '\\'-> return '\\'
                        _   -> fail "unknown escape"

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> symbol <|> digit)
                let atom = first : rest
                return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
                digits <- many1 digit
                return $ Number $ read digits

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseAtom <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
        Left err -> "Error: " ++ show err
        Right output -> "Found"

main:: IO ()
main = getArgs >>= \(expr:_) -> putStrLn $ readExpr expr

