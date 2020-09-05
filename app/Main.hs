module Main
 ( main
 ) where

import Parsing.ParseInput (parseInputStr)

main :: IO ()
main = do
  line <- getLine
  print $ parseInputStr line