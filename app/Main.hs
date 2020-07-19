module Main where

import System.Environment

import Lib

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Hello, " ++ show (sumStrings args)

sumStrings :: [String] -> Int
sumStrings = sum . map read

