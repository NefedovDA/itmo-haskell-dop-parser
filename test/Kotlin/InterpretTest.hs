{-# LANGUAGE LambdaCase #-}

module Kotlin.InterpretTest
  ( testInterpret
  ) where

import System.Directory (removeFile)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=), assertFailure)

import Kotlin.Interpret
import Kotlin.TestTemplate
import Parsing.KotlinPsi (transform)
import Data.Maybe (catMaybes)
import System.IO (openFile, IOMode(..), hGetContents, hClose, hPutStr)
import Control.Exception (evaluate, catch, SomeException)
import Control.DeepSeq (rnf)

testInterpret :: TestTree
testInterpret = testGroup "Testing Interpreter module"
  [ runTests
  ]

runTests :: TestTree
runTests = testGroup "Test interpreting" $
  runTest <$> testTemplates
  where
    runTest :: TestTemplate -> TestTree
    runTest
      tt@TestTemplate
        { ttName = name
        , ttInterpreted =
            InterpretResult
              { irErrors = errors
              , irOutput = output
              }
        }
        =
      testCase name $ do
        let path = "./" ++ map (\c -> if c == ' ' then '_' else c) name ++ ".txt"
        file <- openFile path WriteMode
        hPutStr file ""
        hClose file
        catch (runInterpret tt path) $ \msg ->
          show (msg::SomeException) @?= errors
        file <- openFile path ReadMode
        s <- hGetContents file
        evaluate (rnf s)
        hClose file
        removeFile path
        s @?= output

    runInterpret :: TestTemplate -> String -> IO ()
    runInterpret tt = hioIO . interpret . transform $ ttPsi tt
