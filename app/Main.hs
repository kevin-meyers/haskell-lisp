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

isBool :: LispVal -> ThrowsError LispVal
isBool (Bool _) = return $ Bool True
isBool _ = return $ Bool False

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

main :: IO ()
main = do
     args <- getArgs
     let evaled = fmap show $ readExpr (head args) >>= eval
     putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args) 
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("Boolean?", isBool . head )
            ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op xs@[_] = throwError $ NumArgs 2 xs
numericBinop op params = (Number . foldl1 op) <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- | npackNum notNum = throwError $ TypeMismatch "number" notNum

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
