module Main where

import System.Environment

import Lib

main :: IO ()
main = do
    putStrLn "Who are you?"
    name <- getLine
    putStrLn $ "Hello, " ++ name
