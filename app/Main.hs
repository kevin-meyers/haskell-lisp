module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char (isDigit, digitToInt)

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

convertBase :: String -> Int -> (Char -> Int) -> Int
convertBase xs base convF = foldr (\(x, i) acc -> convF x * base^i + acc) 0 $ zip xs [l-1, l-2 .. 0]
    where
        l = length xs


hexToDecimal :: Char -> Int
hexToDecimal c
    | isDigit c = digitToInt c
hexToDecimal 'A' = 10
hexToDecimal 'B' = 11
hexToDecimal 'C' = 12
hexToDecimal 'D' = 13
hexToDecimal 'E' = 14
hexToDecimal 'F' = 15

parseHex :: Parser Int
parseHex = do
    string "#x"
    xs <- many1 hexDigit
    return $ convertBase xs 16 hexToDecimal

parseBin :: Parser Int
parseBin = do
    string "#b"
    xs <- many1 (oneOf "01")
    return $ convertBase xs 2 digitToInt

parseOct :: Parser Int
parseOct = do
    string "#o"
    xs <- many1 octDigit
    return $ convertBase xs 8 digitToInt

parseDec :: Parser Int
parseDec = do
    optional $ string "#d"
    xs <- many1 digit
    return $ convertBase xs 10 digitToInt

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
