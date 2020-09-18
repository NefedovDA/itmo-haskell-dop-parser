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
import Control.Exception (evaluate)
import Control.DeepSeq (rnf)

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
            let path = "./" ++ map (\c -> if c == ' ' then '_' else c) name ++ ".txt"
            runInterpret tt path
            file <- openFile path ReadMode
            s <- hGetContents file
            evaluate (rnf s)
            hClose file
            removeFile path
            s @?= msg

    runInterpret :: TestTemplate -> String -> IO ()
    runInterpret tt = hioIO . interpret . transform $ ttPsi tt
