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

-- | Base parser of code string to KotlinPsi.
parseInput :: String -> KotlinPsi (KtFile IO)
parseInput input =
  case happyParserExpression $ alexScanTokens input of
    Ok file    -> file
    Failed msg -> error msg

-- | Parse -> Transform to Printer -> print
parseInputStr :: String -> String
parseInputStr = show . transform @Printer . parseInput

-- | Parse -> Transform to Interpret -> interpret
parseInputExe :: String -> IO ()
parseInputExe = interpret . transform . parseInput
