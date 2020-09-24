{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Parsing.ParserTest
  ( testParser
  ) where

import Data.Functor     ((<&>))
import Data.Typeable    (Typeable)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Kotlin.Language ((.:))
import Kotlin.Printer
import Parsing.KotlinPsi
import Parsing.ParseInput (parseInput)

-- | Test group of Parsing.Parser module.
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
      , ttPsi = KtPsiFile []
      }
  , TestTemplate
      { ttName = "Single function"
      , ttNeedSplit = False
      , ttPsi = KtPsiFile
          [ KtPsiFun "f"
            []
            (KtAnyType KtUnitType)
            []
          ]
      }
  , TestTemplate
      { ttName = "Single function with return"
      , ttNeedSplit = False
      , ttPsi = KtPsiFile
          [ KtPsiFun "f"
            []
            (KtAnyType KtUnitType)
            [ KtPsiReturn $ KtPsiUnit () ]
          ]
      }
  , TestTemplate
      { ttName = "Several functions"
      , ttNeedSplit = False
      , ttPsi = KtPsiFile
          [ KtPsiFun "f"
              []
              (KtAnyType KtUnitType)
              []
          , KtPsiFun "g"
              ["a" .: KtAnyType KtIntType ]
              (KtAnyType KtUnitType)
              []
          , KtPsiFun "h"
              [ "a" .: KtAnyType KtStringType
              , "b" .: KtAnyType KtDoubleType
              ]
              (KtAnyType KtUnitType)
              []
          ]
      }
  , TestTemplate
      { ttName = "Arifmetic"
      , ttNeedSplit = True
      , ttPsi = KtPsiFile
          [ testUnit "return Int" $
              KtPsiInt 1
          , testUnit "return Double" $
              KtPsiDouble 1.2
          , testUnit "check Addition" $
              KtPsiInt 1 :+: KtPsiDouble 1.2
          , testUnit "check Different" $
              KtPsiInt 1 :-: KtPsiDouble 1.2
          , testUnit "check Multiplication" $
              KtPsiInt 1 :*: KtPsiDouble 1.2
          , testUnit "check Ratio" $
              KtPsiInt 1 :/: KtPsiDouble 1.2
          , testUnit "check Negate" $
              KtPsiNegate $ KtPsiInt 1
          ]
      }
  , TestTemplate
      { ttName = "Logic operations"
      , ttNeedSplit = True
      , ttPsi = KtPsiFile
          [ testUnit "return True" $
              KtPsiBool True
          , testUnit "return False" $
              KtPsiBool False
          , testUnit "check Or" $
              KtPsiBool False :||: KtPsiBool True
          , testUnit "check And" $
              KtPsiBool False :&&: KtPsiBool True
          , testUnit "check Not" $
              KtPsiNot $ KtPsiBool True
          , testUnit "check Eq numbers" $
              KtPsiInt 1 :==: KtPsiInt 1
          , testUnit "check Lt numbers" $
              KtPsiInt 1 :<: KtPsiInt 1
          , testUnit "check Lte numbers" $
              KtPsiInt 1 :<=: KtPsiInt 1
          , testUnit "check Gt numbers" $
              KtPsiInt 1 :>: KtPsiInt 1
          , testUnit "check Gte numbers" $
              KtPsiInt 1 :>=: KtPsiInt 1
          ]
      }
  ]

testUnit :: Name -> KotlinPsi (KtValue IO) -> KotlinPsi (KtFunData IO)
testUnit name value =
  KtPsiFun (name <&> \case { ' ' -> '_' ; a -> a })
    []
    (KtAnyType KtUnitType)
    [ KtPsiValueCommand value ]

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
    splitTree (KtPsiFile declarations) =
      declarations <&> \f@(KtPsiFun name _ _ _) ->
        TestTemplate
          { ttName = name
          , ttNeedSplit = False
          , ttPsi = KtPsiFile [f]
          }
