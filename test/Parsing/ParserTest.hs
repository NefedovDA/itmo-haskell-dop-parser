{-# LANGUAGE GADTs #-}

module Parsing.ParserTest
  ( testParser
  ) where

import Data.Typeable (Typeable)
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
  { ttName      :: String
  , ttNeedSplit :: Bool
  , ttPsi       :: KotlinPsi (KtFile IO)
  }

testTemplates :: [TestTemplate]
testTemplates =
  [ TestTemplate
      { ttName = "Empty file"
      , ttNeedSplit = False
      , ttPsi = KtPsiFile
          emptyDeclarations
      }
  , TestTemplate
      { ttName = "Single function"
      , ttNeedSplit = False
      , ttPsi = KtPsiFile
          emptyDeclarations
            { kdFun0 =
                [ KtPsiFun0 "f"
                  (KtAnyType KtUnitType)
                  []
                ]
            }
      }
  , TestTemplate
      { ttName = "Single function with return"
      , ttNeedSplit = False
      , ttPsi = KtPsiFile
          emptyDeclarations
            { kdFun0 =
                [ KtPsiFun0 "f"
                  (KtAnyType KtUnitType)
                  [ KtPsiReturn $ KtPsiUnit () ]
                ]
            }
      }
  , TestTemplate
      { ttName = "Several functions"
      , ttNeedSplit = False
      , ttPsi = KtPsiFile
          KtDeclarations
            { kdFun0 =
                [ KtPsiFun0 "f"
                    (KtAnyType KtUnitType)
                    []
                ]
            , kdFun1 =
                [ KtPsiFun1 "g"
                    ("a", (KtAnyType KtIntType))
                    (KtAnyType KtUnitType)
                    []
                ]
            , kdFun2 =
                [ KtPsiFun2 "h"
                    ("a", (KtAnyType KtStringType))
                    ("b", (KtAnyType KtDoubleType))
                    (KtAnyType KtUnitType)
                    []
                ]
            }
      }
  , TestTemplate
      { ttName = "Arifmetic"
      , ttNeedSplit = True
      , ttPsi = KtPsiFile
          emptyDeclarations
            { kdFun0 =
                [ KtPsiFun0 "check_return_Int"
                    (KtAnyType KtIntType)
                    [ KtPsiReturn $ KtPsiInt 1 ]
                , KtPsiFun0 "check_return_Double"
                    (KtAnyType KtDoubleType)
                    [ KtPsiReturn $ KtPsiDouble 1.2 ]
                , KtPsiFun0 "check_Addition"
                    (KtAnyType KtDoubleType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiAddition` KtPsiDouble 1.2
                    ]
                , KtPsiFun0 "check_Different"
                    (KtAnyType KtDoubleType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiDifferent` KtPsiDouble 1.2
                    ]
                , KtPsiFun0 "check_Multiplication"
                    (KtAnyType KtDoubleType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiMultiplication` KtPsiDouble 1.2
                    ]
                , KtPsiFun0 "check_Ratio"
                    (KtAnyType KtDoubleType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiRatio` KtPsiDouble 1.2
                    ]
                , KtPsiFun0 "check_Negate"
                    (KtAnyType KtIntType)
                    [ KtPsiReturn $
                        KtPsiNegate $ KtPsiInt 1
                    ]
                ]
            }
      }
  , TestTemplate
      { ttName = "Logic operations"
      , ttNeedSplit = True
      , ttPsi = KtPsiFile
          emptyDeclarations
            { kdFun0 =
                [ KtPsiFun0 "check_return_True"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $ KtPsiBool True ]
                , KtPsiFun0 "check_return_False"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $ KtPsiBool False ]
                , KtPsiFun0 "check_Or"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiBool False `KtPsiOr` KtPsiBool True
                    ]
                , KtPsiFun0 "check_And"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiBool False `KtPsiOr` KtPsiBool True
                    ]
                , KtPsiFun0 "check_Not"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiNot $ KtPsiBool True
                    ]
                , KtPsiFun0 "check_Eq_numbers"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiEq` KtPsiInt 1
                    ]
                , KtPsiFun0 "check_Lt_numbers"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiLt` KtPsiInt 1
                    ]
                , KtPsiFun0 "check_Lte_numbers"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiLte` KtPsiInt 1
                    ]
                , KtPsiFun0 "check_Gt_numbers"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiGt` KtPsiInt 1
                    ]
                , KtPsiFun0 "check_Gte_numbers"
                    (KtAnyType KtBoolType)
                    [ KtPsiReturn $
                        KtPsiInt 1 `KtPsiGte` KtPsiInt 1
                    ]
                ]
            }
      }
  ]

emptyDeclarations :: KtDeclarations KotlinPsi IO
emptyDeclarations =
  KtDeclarations
    { kdFun0 = []
    , kdFun1 = []
    , kdFun2 = []
    }

type PsiDecl = KtDeclarations KotlinPsi IO
type PsiFunData fun = KotlinPsi (KtFunData fun)

runTests :: TestTree
runTests = testGroup "Test parsing psi" $
  runTest <$> testTemplates
  where
    runTest :: TestTemplate -> TestTree
    runTest TestTemplate { ttName = name, ttNeedSplit = needSplit, ttPsi = psi } =
      if needSplit
      then
        testGroup name $
          runTest <$> splitTree psi
      else
        testCase name $
          parseInput (show psi) @?= psi

    splitTree :: KotlinPsi (KtFile IO) -> [TestTemplate]
    splitTree (KtPsiFile declarations) = concat $
      [ splitDeclarations kdFun0 $ \decl f -> decl { kdFun0 = [f] }
      , splitDeclarations kdFun1 $ \decl f -> decl { kdFun1 = [f] }
      , splitDeclarations kdFun2 $ \decl f -> decl { kdFun2 = [f] }
      ] <*> [declarations]

    splitDeclarations
      :: (PsiDecl -> [PsiFunData fun])
      -> (PsiDecl -> PsiFunData fun -> PsiDecl)
      -> PsiDecl
      -> [TestTemplate]
    splitDeclarations getFuns putFun declarations =
      flip fmap (getFuns declarations) $ \f ->
        TestTemplate
          { ttName =
              case f of
                KtPsiFun0 name _ _     -> name
                KtPsiFun1 name _ _ _   -> name
                KtPsiFun2 name _ _ _ _ -> name
          , ttNeedSplit = False
          , ttPsi = KtPsiFile $ putFun emptyDeclarations f
          }
