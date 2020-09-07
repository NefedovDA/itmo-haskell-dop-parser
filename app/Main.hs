module Main
 ( main
 ) where

import Parsing.ParseInput (parseInputStr, parseInputExe)

main :: IO ()
main = do
  line <- getLine
  putStrLn "Code:"
  putStrLn $ parseInputStr line
  putStrLn "Evaluation:"
  parseInputExe line
