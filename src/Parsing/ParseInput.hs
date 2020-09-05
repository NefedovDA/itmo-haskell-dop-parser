{-# LANGUAGE GADTs #-}

module Parsing.ParseInput
  ( parseString
  , parseInput
  , parseInputStr
  ) where

import           Parsing.Expression
import           Parsing.Lexer
import           Parsing.Parser
import           Parsing.ParserHelper

parseString :: String -> Result (KotlinPsi KtFile)
parseString input = happyParserExpression $ alexScanTokens input

transform :: Kotlin expr => KotlinPsi a -> expr a
transform a = case a of
  KtPsiFile list  -> ktFile (map transform list)
  KtPsiFun0Unit n -> ktFun0Unit n

parseInput :: Kotlin expr => String -> expr KtFile
parseInput input =
  case result of
    Ok arr     -> arr
    Failed msg -> error msg
  where
    result = transform <$> parseString input

parseInputStr :: String -> ToS KtFile
parseInputStr = parseInput