{-# LANGUAGE GADTs #-}

module Parsing.ParseInput
  ( parseInput
  , parseInputExe
  , parseInputStr
  ) where

import Kotlin.Dsl         (Kotlin, KtFile)
import Kotlin.Interpreter (interpret)
import Kotlin.Printer     (runPrint)
import Parsing.KotlinPsi  (transform)
import Parsing.Lexer      (alexScanTokens)
import Parsing.Parser     (happyParserExpression)
import Parsing.Result     (Result(..))

parseInput :: Kotlin expr => String -> expr KtFile
parseInput input =
  case result of
    Ok file    -> file
    Failed msg -> error msg
  where
    result :: Kotlin expr => Result (expr KtFile)
    result = transform <$> (happyParserExpression $ alexScanTokens input)

parseInputStr :: String -> String
parseInputStr = runPrint . parseInput

parseInputExe :: String -> IO ()
parseInputExe = interpret . parseInput