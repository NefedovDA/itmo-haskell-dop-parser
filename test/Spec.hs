import Test.Tasty (testGroup, defaultMain)

import Parsing.LexerTest (testLexer)
import Parsing.ParserTest (testParser)

main :: IO ()
main = defaultMain $ testGroup "All tests" 
  [ testLexer
  , testParser
  ]
