{-# LANGUAGE TypeApplications #-}

module Parsing.ParseInput
  ( parseInput
  , parseInputExe
  , parseInputStr
  ) where

import Kotlin.Dsl        (KtFile)
import Kotlin.Interpret  (interpret)
import Kotlin.Printer    (Printer)
import Parsing.KotlinPsi (KotlinPsi, transform)
import Parsing.Lexer     (alexScanTokens)
import Parsing.Parser    (happyParserExpression)
import Parsing.Result    (Result(..))

parseInput :: String -> KotlinPsi (KtFile IO)
parseInput input =
  case happyParserExpression $ alexScanTokens input of
    Ok file    -> file
    Failed msg -> error msg

parseInputStr :: String -> String
parseInputStr = show . transform @Printer <$> parseInput

parseInputExe :: String -> IO ()
parseInputExe = interpret . transform <$> parseInput
