module Main
 ( main
 ) where

import Parsing.Lexer

main :: IO ()
main = do
  line <- getLine
  print $ alexScanTokens line
