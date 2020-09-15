{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Parsing.KotlinPsi
  ( module Kotlin.Dsl

  , KotlinPsi(..)

  , transform
  ) where

import Data.Typeable ((:~:)(..), eqT, Typeable)

import Kotlin.Dsl
import Kotlin.Printer (runPrint)

data KotlinPsi a where
  KtPsiFile :: KtDeclarations KotlinPsi -> KotlinPsi KtFile

  KtPsiFun0
    :: Name
    -> KtAnyType
    -> [KotlinPsi KtCommand]
    -> KotlinPsi (KtFunData KtFun0)
  KtPsiFun1
    :: Name
    -> KtFunArg
    -> KtAnyType
    -> [KotlinPsi KtCommand]
    -> KotlinPsi (KtFunData KtFun1)
  KtPsiFun2
    :: Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [KotlinPsi KtCommand]
    -> KotlinPsi (KtFunData KtFun2)

  KtPsiReturn :: KotlinPsi KtValue -> KotlinPsi KtCommand
  KtPsiValueCommand :: KotlinPsi KtValue -> KotlinPsi KtCommand
  
  KtPsiCallFun0 :: Name -> KotlinPsi KtValue
  KtPsiCallFun1 :: Name -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiCallFun2 :: Name -> KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue

  KtPsiAddition :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiDifferent :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiMultiplication :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiRatio :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiNegate :: KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiAnd :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiOr :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiNot :: KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiEq :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiNotEq :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiGt :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiGte :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiLt :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue
  KtPsiLte :: KotlinPsi KtValue -> KotlinPsi KtValue -> KotlinPsi KtValue

  KtPsiInt    :: Int    -> KotlinPsi KtValue
  KtPsiDouble :: Double -> KotlinPsi KtValue
  KtPsiString :: String -> KotlinPsi KtValue
  KtPsiBool   :: Bool   -> KotlinPsi KtValue
  KtPsiUnit   :: ()     -> KotlinPsi KtValue

instance Eq (KtDeclarations KotlinPsi) where
  (==) :: KtDeclarations KotlinPsi -> KtDeclarations KotlinPsi -> Bool
  decL == decR =
    (kdFun0 decL == kdFun0 decR) &&
    (kdFun1 decL == kdFun1 decR) &&
    (kdFun2 decL == kdFun2 decR)

instance Eq KtAnyType where
  (==) :: KtAnyType -> KtAnyType -> Bool
  KtAnyType (_ :: KtType a) == KtAnyType (_ ::KtType b) =
    case eqT @a @b of
      Nothing -> False
      Just _  -> True

instance Eq (KotlinPsi a) where
  (==) :: KotlinPsi a -> KotlinPsi a -> Bool
  KtPsiFile fDecL == KtPsiFile fDecR
    = fDecL == fDecR

  KtPsiFun0 nameL rTypeL cmdsL == KtPsiFun0 nameR rTypeR cmdsR =
    (nameL == nameR)
      && (rTypeL == rTypeR)
      && (cmdsL == cmdsR)
  KtPsiFun1 nameL argL rTypeL cmdsL == KtPsiFun1 nameR argR rTypeR cmdsR =
    (nameL == nameR)
      && (argL == argR)
      && (rTypeL == rTypeR)
      && (cmdsL == cmdsR)
  KtPsiFun2 nameL arg1L arg2L rTypeL cmdsL == KtPsiFun2 nameR arg1R arg2R rTypeR cmdsR =
    (nameL == nameR)
      && (arg1L == arg1R)
      && (arg2L == arg2R)
      && (rTypeL == rTypeR)
      && (cmdsL  == cmdsR)

  KtPsiReturn valueL == KtPsiReturn valueR = valueL == valueR
  KtPsiValueCommand valueL == KtPsiValueCommand valueR = valueL == valueR
  
  KtPsiCallFun0 nameL == KtPsiCallFun0 nameR = nameL == nameR
  KtPsiCallFun1 nameL aL == KtPsiCallFun1 nameR aR =
    (nameL == nameR)
      && (aL == aR)
  KtPsiCallFun2 nameL a1L a2L == KtPsiCallFun2 nameR a1R a2R =
    (nameL == nameR)
      && (a1L == a1R)
      && (a2L == a2R)

  KtPsiAddition lvL rvL == KtPsiAddition lvR rvR =
    (lvL == lvR) && (rvL == rvL)
  KtPsiDifferent lvL rvL == KtPsiDifferent lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiMultiplication lvL rvL == KtPsiMultiplication lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiRatio lvL rvL == KtPsiRatio lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiNegate vL == KtPsiNegate vR =
    vL == vR
  KtPsiAnd lvL rvL == KtPsiAnd lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiOr lvL rvL == KtPsiOr lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiNot vL == KtPsiNot vR
    = vL == vR
  KtPsiEq lvL rvL == KtPsiEq lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiNotEq lvL rvL == KtPsiNotEq lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiGt lvL rvL == KtPsiGt lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiGte lvL rvL == KtPsiGte lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiLt lvL rvL == KtPsiLt lvR rvR =
    (lvL == lvR) && (rvL == rvR)
  KtPsiLte  lvL rvL == KtPsiLte  lvR rvR =
    (lvL == lvR) && (rvL == rvR)

  KtPsiInt iL == KtPsiInt iR = iL == iR
  KtPsiDouble dL == KtPsiDouble dR = dL == dR
  KtPsiString sL == KtPsiString sR = sL == sR
  KtPsiBool bL == KtPsiBool bR = bL == bR
  KtPsiUnit () == KtPsiUnit () = True

  _ == _ = False

instance Show (KotlinPsi a) where
  show :: KotlinPsi a -> String
  show = runPrint . transform

transform :: Kotlin expr => KotlinPsi a -> expr a
transform a = case a of
  KtPsiFile dec -> ktFile $ KtDeclarations
    { kdFun0 = transform <$> kdFun0 dec  
    , kdFun1 = transform <$> kdFun1 dec  
    , kdFun2 = transform <$> kdFun2 dec  
    }

  KtPsiFun0 n t cs     -> ktFun0 n t $ transform <$> cs
  KtPsiFun1 n a t cs   -> ktFun1 n a t $ transform <$> cs
  KtPsiFun2 n a b t cs -> ktFun2 n a b t $ transform <$> cs

  KtPsiReturn r -> ktReturn $ transform r
  KtPsiValueCommand r -> ktValueCommand $ transform r
  
  KtPsiCallFun0 n -> ktCallFun0 n
  KtPsiCallFun1 n a -> ktCallFun1 n $ transform a
  KtPsiCallFun2 n a1 a2 -> ktCallFun2 n (transform a1) (transform a2)

  KtPsiAddition lv rv       -> ktAddition (transform lv) (transform rv)
  KtPsiDifferent lv rv      -> ktDifferent (transform lv) (transform rv)
  KtPsiMultiplication lv rv -> ktMultiplication (transform lv) (transform rv)
  KtPsiRatio lv rv          -> ktRatio (transform lv) (transform rv)
  KtPsiAnd lv rv            -> ktAnd (transform lv) (transform rv)
  KtPsiOr lv rv             -> ktOr (transform lv) (transform rv)
  KtPsiEq lv rv             -> ktEq (transform lv) (transform rv)
  KtPsiNotEq lv rv          -> ktNotEq (transform lv) (transform rv)
  KtPsiGt lv rv             -> ktGt (transform lv) (transform rv)
  KtPsiGte lv rv            -> ktGte (transform lv) (transform rv)
  KtPsiLt lv rv             -> ktLt (transform lv) (transform rv)
  KtPsiLte lv rv            -> ktLte (transform lv) (transform rv)

  KtPsiNot v    -> ktNot $ transform v
  KtPsiNegate v -> ktNegate $ transform v

  KtPsiInt i    -> ktInt i   
  KtPsiDouble d -> ktDouble d
  KtPsiString s -> ktString s
  KtPsiBool b   -> ktBool b  
  KtPsiUnit ()  -> ktUnit ()  
