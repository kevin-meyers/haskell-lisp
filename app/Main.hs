module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Number (binary)
import System.Environment

import Lib


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

           
main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ noneOf "\"" <|> (oneOf "\\" >> oneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = string "#x" >> many1 hexDigit
            <|> string "#o" >> many1 octDigit
            <|> string "#b" >> many1 binary
Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
