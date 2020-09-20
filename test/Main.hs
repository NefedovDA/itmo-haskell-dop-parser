module Main
  (main
  ) where

import Test.Tasty (testGroup, defaultMain)

import Kotlin.Spec  (testKotlin)
import Parsing.Spec (testParsing)

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ testKotlin
  , testParsing
  ]
