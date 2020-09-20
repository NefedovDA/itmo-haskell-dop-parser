module Kotlin.InterpretTest
  ( testInterpret
  ) where

import Control.DeepSeq   (rnf)
import Control.Exception (evaluate, catch, SomeException)
import System.Directory  (removeFile)
import System.IO         (openFile, IOMode(..), hGetContents, hClose, hPutStr)
import Test.Tasty        (TestTree, testGroup)
import Test.Tasty.HUnit  (Assertion, testCase, (@?=), assertFailure)

import Kotlin.Interpret

import Kotlin.TestTemplate

-- | Test group of the Kotlin.Interpret module.
testInterpret :: TestTree
testInterpret = testGroup "Testing Interpreter module"
  [ runTests
  ]

runTests :: TestTree
runTests = testGroup "Test interpreting" $
  runTest <$> testTemplates
  where
    runTest :: TestTemplate Interpret -> TestTree
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

    runInterpret :: TestTemplate Interpret -> String -> IO ()
    runInterpret = hioIO . interpret . ttPsi
