module Parsing.ParseInput
  ( parseInput
  , parseInputExe
  , parseInputStr
  ) where

import Kotlin.Dsl         (Kotlin, KtFile)
import Kotlin.Interpreter (interpret)
import Kotlin.Printer     (runPrint)
import Parsing.KotlinPsi  (KotlinPsi, transform)
import Parsing.Lexer      (alexScanTokens)
import Parsing.Parser     (happyParserExpression)
import Parsing.Result     (Result(..))

parseInput :: String -> KotlinPsi KtFile
parseInput input =
  case happyParserExpression $ alexScanTokens input of
    Ok file    -> file
    Failed msg -> error msg

parseInputStr :: String -> String
parseInputStr = runPrint . transform <$> parseInput

parseInputExe :: String -> IO ()
parseInputExe = interpret . transform <$> parseInput