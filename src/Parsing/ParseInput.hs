module Parsing.ParseInput
  ( parseInput
  , parseString
  ) where

import           Parsing.Expression
import           Parsing.Lexer
import           Parsing.Parser
import           Parsing.ParserHelper

parseString :: String -> Result File
parseString input = happyParserExpression $ alexScanTokens input

parseInput :: String -> File
parseInput input =
  case result of
    Ok arr     -> arr
    Failed msg -> error msg
  where
    result = parseString input