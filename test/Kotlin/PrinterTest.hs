module Kotlin.PrinterTest
  ( testPrinter
  ) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Kotlin.TestTemplate

testPrinter :: TestTree
testPrinter = testGroup "Testing Printer module"
  [ runTests
  ]

runTests :: TestTree
runTests = testGroup "Test printing" $
  runTest <$> testTemplates
  where
    runTest tt@TestTemplate { ttName = name, ttPrinted = expected } =
      testCase name $ runPrinter tt @?= expected
    
    runPrinter :: TestTemplate -> String
    runPrinter = show . ttPsi