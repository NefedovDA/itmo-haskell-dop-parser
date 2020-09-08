module Parsing.ParserTest
  ( testParser
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Kotlin.Dsl
import Kotlin.Printer
import Parsing.KotlinPsi
import Parsing.ParseInput (parseInput)

testParser :: TestTree
testParser = testGroup "Test Parser module"
  [ runTests
  ]

data TestTemplate = TestTemplate
  { ttName :: String
  , ttPsi  :: KotlinPsi KtFile
  }

testTemplates :: [TestTemplate]
testTemplates =
  [ TestTemplate
      { ttName = "Empty file"
      , ttPsi = KtPsiFile
          FunDecl 
            { fdFun0 = []
            , fdFun1 = []
            , fdFun2 = []
            }
      }
  , TestTemplate
      { ttName = "Single function"
      , ttPsi = KtPsiFile
          FunDecl 
            { fdFun0 = [ KtPsiFun0 "f" KtUnitType ]
            , fdFun1 = []
            , fdFun2 = []
            }
      }
  , TestTemplate
      { ttName = "Several functions"
      , ttPsi = KtPsiFile
          FunDecl 
            { fdFun0 = [ KtPsiFun0 "f" KtUnitType ]
            , fdFun1 = [ KtPsiFun1 "g" ("a", KtIntType) KtIntType ]
            , fdFun2 = [ KtPsiFun2 "h" ("a", KtStringType) ("b", KtStringType) KtDoubleType ]
            }
      }
  ]

runTests :: TestTree
runTests = testGroup "Test parsing psi" $
  runTest <$> testTemplates
  where
    runTest :: TestTemplate -> TestTree
    runTest TestTemplate { ttName = name, ttPsi = psi } = testCase name $
      parseInput (show psi) @?= psi