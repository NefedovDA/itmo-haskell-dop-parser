module Parsing.LexerTest
  ( testLexer
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Parsing.Lexer

-- | Test group of the Parsing.Lexer module.
testLexer :: TestTree
testLexer = testGroup "Test Lexer module"
  [ testKeys
  , testSymbols
  , testNames
  , testDigits
  , testStrings
  , testComments
  , testSpecificFunctions
  ]

checkSingleTokenLexing :: String -> TokenType -> Assertion
checkSingleTokenLexing str tokenType =
  alexScanTokens str @?= [Token tokenType (AlexPn 0 1 1) str]

testKeys :: TestTree
testKeys = testGroup "Test lexing key words"
  [ testCase "Lexing true"   $ checkSingleTokenLexing "true"   KeyTrue
  , testCase "Lexing false"  $ checkSingleTokenLexing "false"  KeyFalse

  , testCase "Lexing val"    $ checkSingleTokenLexing "val"    KeyVal
  , testCase "Lexing var"    $ checkSingleTokenLexing "var"    KeyVar

  , testCase "Lexing fun"    $ checkSingleTokenLexing "fun"    KeyFun

  , testCase "Lexing return" $ checkSingleTokenLexing "return" KeyReturn

  , testCase "Lexing for"    $ checkSingleTokenLexing "for"    KeyFor
  , testCase "Lexing in"     $ checkSingleTokenLexing "in"     KeyIn

  , testCase "Lexing if"     $ checkSingleTokenLexing "if"     KeyIf
  , testCase "Lexing else"   $ checkSingleTokenLexing "else"   KeyElse
  ]

testTypes :: TestTree
testTypes = testGroup "Test lexing types"
  [ testCase "Lexing Unit"   $ checkSingleTokenLexing "Unit"   TypeUnit
  , testCase "Lexing Int"    $ checkSingleTokenLexing "Int"    TypeUnit
  , testCase "Lexing Double" $ checkSingleTokenLexing "Double" TypeUnit
  , testCase "Lexing Bool"   $ checkSingleTokenLexing "Bool"   TypeUnit
  , testCase "Lexing String" $ checkSingleTokenLexing "String" TypeUnit
  ]

testSymbols :: TestTree
testSymbols = testGroup "Test lexing symbols"
  [ testCase "Lexing '('"  $ checkSingleTokenLexing "("  OCBracket
  , testCase "Lexing ')'"  $ checkSingleTokenLexing ")"  CCBracket

  , testCase "Lexing '{'"  $ checkSingleTokenLexing "{"  OBBracket
  , testCase "Lexing '}'"  $ checkSingleTokenLexing "}"  CBBracket

  , testCase "Lexing '='"  $ checkSingleTokenLexing "="  Equals
  , testCase "Lexing ':'"  $ checkSingleTokenLexing ":"  Colon
  , testCase "Lexing ';'"  $ checkSingleTokenLexing ";"  IEnd
  , testCase "Lexing ','"  $ checkSingleTokenLexing ","  Comma
  , testCase "Lexing '..'" $ checkSingleTokenLexing ".." DPoint
  ]

testOperations :: TestTree
testOperations = testGroup "Test lexing operations"
  [ testCase "Lexing '+'"   $ checkSingleTokenLexing "+"   Plus
  , testCase "Lexing '-'"   $ checkSingleTokenLexing "-"   Minus
  , testCase "Lexing '*'"   $ checkSingleTokenLexing "*"   Mull
  , testCase "Lexing '/'"   $ checkSingleTokenLexing "/"   Div

  , testCase "Lexing '&&'"  $ checkSingleTokenLexing "&&"  And
  , testCase "Lexing '||'"  $ checkSingleTokenLexing "||"  Or
  , testCase "Lexing '!'"   $ checkSingleTokenLexing "!"   Not

  , testCase "Lexing '=='"  $ checkSingleTokenLexing "=="  Eq
  , testCase "Lexing '!='"  $ checkSingleTokenLexing "!="  NotEq

  , testCase "Lexing '>='"  $ checkSingleTokenLexing ">="  Gte
  , testCase "Lexing '<='"  $ checkSingleTokenLexing "<="  Lte
  , testCase "Lexing '>'"   $ checkSingleTokenLexing ">"   Gt
  , testCase "Lexing '<'"   $ checkSingleTokenLexing "<"   Lt
  ]

testNames :: TestTree
testNames = testGroup "Test lexing words"
  [ testCase "Lexing name 'a'"   $ checkSingleTokenLexing "a"   Name
  , testCase "Lexing name 'aaa'" $ checkSingleTokenLexing "aaa" Name
  , testCase "Lexing name '_12'" $ checkSingleTokenLexing "_12" Name
  ]

testDigits :: TestTree
testDigits = testGroup "Test lexing digits"
  [ testCase "Lexing int '1'"       $ checkSingleTokenLexing "1"    IntNum
  , testCase "Lexing int '12'"      $ checkSingleTokenLexing "12"   IntNum
  , testCase "Lexing int '0'"       $ checkSingleTokenLexing "0"    IntNum

  , testCase "Lexing double '1.0'"  $ checkSingleTokenLexing "1.0"  DoubleNum
  , testCase "Lexing double '12.0'" $ checkSingleTokenLexing "12.0" DoubleNum
  , testCase "Lexing double '0.0'"  $ checkSingleTokenLexing "0.0"  DoubleNum
  , testCase "Lexing double '1.23'" $ checkSingleTokenLexing "1.23" DoubleNum
  ]

testStrings :: TestTree
testStrings = testGroup "Test lexing strings"
  [ testCase "Lexing string \"\""    $ checkSingleTokenLexing "\"\""    Str
  , testCase "Lexing string \"abc\"" $ checkSingleTokenLexing "\"abc\"" Str
  ]

testComments :: TestTree
testComments = testGroup "Test lexing comments"
  [ testCase "Lexing comment /**/"        $ alexScanTokens "/**/"        @?= []
  , testCase "Lexing comment /* 1 + 1 */" $ alexScanTokens "/* 1 + 1 */" @?= []
  ]

testSpecificFunctions :: TestTree
testSpecificFunctions = testGroup "Test lexing words"
  [ testCase "Lexing name 'readLine().toInt'"
      $ checkSingleTokenLexing "readLine().toInt" Name
  , testCase "Lexing name 'readLine().toDouble'"
      $ checkSingleTokenLexing "readLine().toDouble" Name
  ]
