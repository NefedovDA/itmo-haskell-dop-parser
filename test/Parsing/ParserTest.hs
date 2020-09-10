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
          KtDeclarations
            { kdFun0 = []
            , kdFun1 = []
            , kdFun2 = []
            }
      }
  , TestTemplate
      { ttName = "Single function"
      , ttPsi = KtPsiFile
          KtDeclarations
            { kdFun0 = [ KtPsiFun0 "f" (KtAnyType KtUnitType) ]
            , kdFun1 = []
            , kdFun2 = []
            }
      }
  , TestTemplate
      { ttName = "Several functions"
      , ttPsi = KtPsiFile
          KtDeclarations
            { kdFun0 =
                [ KtPsiFun0 "f" 
                    (KtAnyType KtUnitType)
                ]
            , kdFun1 =
                [ KtPsiFun1 "g"
                    ("a", (KtAnyType KtIntType))
                    (KtAnyType KtIntType)
                ]
            , kdFun2 =
                [ KtPsiFun2 "h"
                    ("a", (KtAnyType KtStringType))
                    ("b", (KtAnyType KtStringType))
                    (KtAnyType KtDoubleType)
                ]
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