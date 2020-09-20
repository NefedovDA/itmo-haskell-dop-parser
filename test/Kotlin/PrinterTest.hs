module Kotlin.PrinterTest
  ( testPrinter
  ) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kotlin.Printer (Printer)

import Kotlin.TestTemplate

-- | Test group of the Kotlin.Printer module.
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

    runPrinter :: TestTemplate Printer -> String
    runPrinter = show . ttPsi
