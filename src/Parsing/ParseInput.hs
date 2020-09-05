module Parsing.ParseInput
  ( parseInput
  , parseString
  ) where

import           Parsing.Expression
import           Parsing.Lexer
import           Parsing.Parser
import           Parsing.ParserHelper

parseString :: Kotlin expr => String -> Result (expr KtFile)  
parseString input = happyParserExpression $ alexScanTokens input

parseInput :: Kotlin expr => String -> expr KtFile
parseInput input =
  case result of
    Ok arr     -> arr
    Failed msg -> error msg
  where
    result = parseString input