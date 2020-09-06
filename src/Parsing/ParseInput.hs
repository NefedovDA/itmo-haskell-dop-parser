{-# LANGUAGE GADTs #-}

module Parsing.ParseInput
  ( parseInput
  , parseInputExe
  , parseInputStr
  ) where

import Parsing.Expression   (Kotlin(..), KotlinPsi(..), ToS, Interpret(..), KtFile, transform)
import Parsing.Lexer        (alexScanTokens)
import Parsing.Parser       (happyParserExpression)
import Parsing.ParserHelper (Result(..))

parseInput :: Kotlin expr => String -> expr KtFile
parseInput input =
  case result of
    Ok file    -> file
    Failed msg -> error msg
  where
    result :: Kotlin expr => Result (expr KtFile)
    result = transform <$> (happyParserExpression $ alexScanTokens input)

parseInputStr :: String -> ToS KtFile
parseInputStr = parseInput

parseInputExe :: String -> IO ()
parseInputExe = interpret . parseInput