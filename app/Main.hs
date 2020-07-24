module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char (isDigit, digitToInt, intToDigit)
import Data.List (elemIndex)

import Control.Monad.Except


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

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

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

baseValues :: String
baseValues = ['0'..'9'] ++ ['A'..'Z']

convertBase :: String -> Int -> Int
convertBase xs base = foldr (\(x, i) acc -> baseToDec x * base^i + acc) 0 $ zip xs [l-1, l-2 .. 0]
    where
        l = length xs

baseToDec :: Char -> Int
baseToDec x = valFromBase $ elemIndex x baseValues

valFromBase :: Maybe Int -> Int
valFromBase (Just n) = n
valFromBase Nothing = error "value is larger than max base 36"

parseBase :: String -> Int -> Parser Int
parseBase prefix base = do
    string prefix
    xs <- many1 $ oneOf $ take base baseValues
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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("Boolean?", isBool . head )
            ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op = Number . foldl1 op . map unpackNum

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
npackNum _ = 0
