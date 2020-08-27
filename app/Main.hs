module Main
 ( main
 ) where

import Parsing.ParseInput (parseInput)

main :: IO ()
main = do
  line <- getLine
  print $ parseInput line
