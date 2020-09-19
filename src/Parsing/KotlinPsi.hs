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

import Data.Bifunctor (first, second)
import Data.Typeable ((:~:)(..), eqT, Typeable)

import Kotlin.Dsl
import Kotlin.Printer (Printer)

data KotlinPsi a where
  KtPsiFile :: (Console c) => KtDeclarations KotlinPsi c -> KotlinPsi (KtFile c)

  KtPsiFun
    :: (Console c)
    => Name
    -> [KtFunArg]
    -> KtAnyType
    -> [KotlinPsi (KtCommand c)]
    -> KotlinPsi (KtFunData c)
  
  KtPsiInitVariable
      :: (Console c)
      => Bool
      -> Name
      -> KtAnyType
      -> KotlinPsi (KtValue c)
      -> KotlinPsi (KtCommand c)
    
  KtPsiSetVariable :: (Console c) => Name -> KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)

  KtPsiReturn :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
  KtPsiValueCommand :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtCommand c)
  
  
  KtPsiCallFun :: (Console c) => Name -> [KotlinPsi (KtValue c)] -> KotlinPsi (KtValue c)

  KtPsiReadVariable :: (Console c) => Name -> KotlinPsi (KtValue c)
  
  KtPsiFor
      :: (Console c)
      => Name
      -> KotlinPsi (KtValue c)
      -> KotlinPsi (KtValue c)
      -> [KotlinPsi (KtCommand c)]
      -> KotlinPsi (KtCommand c)

  KtPsiIf
    :: (Console c)
    => [(KotlinPsi (KtValue c), [KotlinPsi (KtCommand c)])]
    -> [KotlinPsi (KtCommand c)]
    -> KotlinPsi (KtCommand c)

  KtPsiAddition :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiDifferent :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiMultiplication :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiRatio :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiNegate :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiAnd :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiOr :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiNot :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiEq :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiNotEq :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiGt :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiGte :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiLt :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiLte :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)

  KtPsiInt :: (Console c) => Int -> KotlinPsi (KtValue c)
  KtPsiDouble :: (Console c) => Double -> KotlinPsi (KtValue c)
  KtPsiString :: (Console c) => String -> KotlinPsi (KtValue c)
  KtPsiBool :: (Console c) => Bool -> KotlinPsi (KtValue c)
  KtPsiUnit :: (Console c) => () -> KotlinPsi (KtValue c)

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

  KtPsiFun nameL argsL rTypeL cmdsL == KtPsiFun nameR argsR rTypeR cmdsR =
    (nameL == nameR)
      && (argsL == argsR)
      && (rTypeL == rTypeR)
      && (cmdsL == cmdsR)

  KtPsiInitVariable isConstantL nameL typeL valueL
    == KtPsiInitVariable isConstantR nameR typeR valueR =
    (isConstantL == isConstantR)
      && (nameL == nameR)
      && (typeL == typeR)
      && (valueL == valueL)

  KtPsiSetVariable nameL valueL == KtPsiSetVariable nameR valueR =
    (nameL == nameR)
      && (valueL == valueL)

  KtPsiReturn valueL == KtPsiReturn valueR = valueL == valueR
  KtPsiValueCommand valueL == KtPsiValueCommand valueR = valueL == valueR

  KtPsiCallFun nameL argsL == KtPsiCallFun nameR argsR =
    (nameL == nameR)
      && (argsL == argsR)

  KtPsiReadVariable nameL == KtPsiReadVariable nameR = nameL == nameR
  
  KtPsiFor nameL fromL toL cmdsL == KtPsiFor nameR fromR toR cmdsR =
    (nameL == nameR)
      && (fromL == fromR)
      && (toL == toR)
      && (cmdsR == cmdsR)
  
  KtPsiIf branchesL elseBranchL == KtPsiIf branchesR elseBranchR =
    (branchesL == branchesR)
      && (branchesL == branchesR)

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
  show = show . transform @Printer

transform :: Kotlin expr => KotlinPsi a -> expr a
transform a = case a of
  KtPsiFile ds -> ktFile $ transform <$> ds

  KtPsiFun n as t cs   -> ktFun n as t $ transform <$> cs
  
  KtPsiInitVariable ic n t v -> ktInitVariable ic n t $ transform v
  KtPsiSetVariable n v       -> ktSetVariable n $ transform v

  KtPsiReturn r -> ktReturn $ transform r
  KtPsiValueCommand r -> ktValueCommand $ transform r
  
  KtPsiCallFun n as -> ktCallFun n $ transform <$> as

  KtPsiReadVariable n  -> ktReadVariable n
  
  KtPsiFor n f t cs -> ktFor n (transform f) (transform t) (transform <$> cs)
  
  KtPsiIf bs eb -> ktIf (first transform . second (transform <$>) <$> bs) (transform <$> eb)

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
