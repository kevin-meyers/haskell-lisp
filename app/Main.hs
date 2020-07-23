module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char (isDigit, digitToInt, intToDigit)

import Lib


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ (oneOf "\\" >> oneOf "\"") <|> noneOf "\""
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

convertBase :: String -> Int -> Int
convertBase xs base = foldr (\(x, i) acc -> baseToDec x * base^i + acc) 0 $ zip xs [l-1, l-2 .. 0]
    where
        l = length xs

baseToDec :: Char -> Int
baseToDec c
    | isDigit c = digitToInt c
baseToDec 'A' = 10
baseToDec 'B' = 11
baseToDec 'C' = 12
baseToDec 'D' = 13
baseToDec 'E' = 14
baseToDec 'F' = 15

parserForBase :: Int -> Parser Char
parserForBase base = oneOf $ take base $ map intToDigit [0..9] ++ ['A'..'Z']

parseBase :: String -> Int -> Parser Int
parseBase prefix base = do
    string prefix
    xs <- many1 $ parserForBase base
    return $ convertBase xs base

parseHex :: Parser Int
parseHex = parseBase "#x" 16

parseBin :: Parser Int
parseBin = parseBase "#b" 2

parseOct :: Parser Int
parseOct = parseBase "#o" 8

parseDec :: Parser Int
parseDec = parseBase "#d" 10 <|> parseBase "" 10

parseNumber :: Parser LispVal
parseNumber = Number . fromIntegral <$> (try parseOct <|> try parseHex <|> try parseBin <|> try parseDec)

parseExpr :: Parser LispVal
parseExpr = parseNumber
        <|> parseAtom
        <|> parseString
        <|> parseQuoted
        <|> parseLists

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseLists :: Parser LispVal
parseLists = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x
