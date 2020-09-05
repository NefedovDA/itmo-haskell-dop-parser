import Test.Tasty (testGroup, defaultMain)

import Parsing.LexerTest (testLexer)

main :: IO ()
main = defaultMain $ testGroup "All tests" 
  [ testLexer
  ]
