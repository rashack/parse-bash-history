#!/usr/bin/env runhaskell

module Main where

import Data.List
import System.Environment
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  args <- getArgs
  ss <- mapM readFile args
  let ps = zipWith (curry sortHist) args ss
  let res = foldr merge [] ps
  mapM_ printHist res

printHist :: Show a => (a, String) -> IO ()
printHist (i, c) = do
  putChar '#'
  print i
  putStrLn c

sortHist :: (FilePath, String) -> [(Int, String)]
sortHist (f, s) =
  case parseFile f s of
    Left e  -> [(0, "Error parsing input:" ++ show e)]
    Right r -> sort r

parseFile :: FilePath -> String -> Either ParseError [(Int, String)]
parseFile = parse bashHistoryFile

bashHistoryFile = many histElem

histElem :: GenParser Char st (Int, String)
histElem = do
  num <- histTime
  cmd <- histCmd
  return (read num :: Int, cmd)

histTime = do
  char '#'
  num <- many1 digit
  newline
  return num

histCmd = do
  cmd <- many1 (noneOf "\n")
  newline
  return cmd

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
