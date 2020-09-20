module Kotlin.Spec
  ( testKotlin
  ) where

import Test.Tasty (TestTree, testGroup)

import Kotlin.InterpretTest (testInterpret)
import Kotlin.PrinterTest   (testPrinter)

-- | Test group of the Kotlin module
testKotlin :: TestTree
testKotlin = testGroup "Test Kotlin module"
  [ testPrinter
  , testInterpret
  ]

