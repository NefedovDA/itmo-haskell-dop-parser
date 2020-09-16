module Kotlin.Spec
  ( testKotlin
  ) where

import Test.Tasty (TestTree, testGroup)

import Kotlin.InterpretTest (testInterpret)
import Kotlin.PrinterTest   (testPrinter)

testKotlin :: TestTree
testKotlin = testGroup "Test Kotlin module"
  [ testPrinter
  , testInterpret
  ]

