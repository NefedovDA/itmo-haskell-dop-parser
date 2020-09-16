{-# LANGUAGE LambdaCase #-}

module Kotlin.InterpretTest
  ( testInterpret
  ) where

import System.Directory (removeFile)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Kotlin.Interpret
import Kotlin.TestTemplate
import Parsing.KotlinPsi (transform)
import Data.Maybe (catMaybes)
import System.IO (openFile, IOMode(..), hGetContents, hClose, hPutStr)

testInterpret :: TestTree
testInterpret = testGroup "Testing Interpreter module"
  [ runTests
  ]

runTests :: TestTree
runTests = testGroup "Test interpreting" $
  catMaybes $ runTest <$> testTemplates
  where
    runTest :: TestTemplate -> Maybe TestTree
    runTest tt@TestTemplate { ttName = name, ttInterpreted = expected } =
      case expected of
        Nothing  -> Nothing
        Just msg -> Just $
          testCase name $ do
            file <- openFile testOutputFile WriteMode
            hPutStr file ""
            hClose file
            runInterpret tt
            file <- openFile testOutputFile ReadMode
            s <- hGetContents file
            s @?= msg
            hClose file

    runInterpret :: TestTemplate -> IO ()
    runInterpret tt = hioIO . interpret . transform $ ttPsi tt
