module Main where

import qualified ParseBashHistory as BH
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  ss <- mapM readBinaryFile args
  let ps = zipWith (curry BH.sortHist) args ss
  let res = foldr BH.merge [] ps
  mapM_ BH.printHist $ BH.uniq res

readBinaryFile file = do
  h <- openFile file ReadMode
  hSetBinaryMode h True
  hGetContents h
