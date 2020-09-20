module Parsing.Spec
  ( testParsing
  ) where

import Test.Tasty (TestTree, testGroup)

import Parsing.LexerTest  (testLexer)
import Parsing.ParserTest (testParser)

-- | Test group of the Parsing module.
testParsing :: TestTree
testParsing = testGroup "Test Parsing module"
  [ testLexer
  , testParser
  ]
