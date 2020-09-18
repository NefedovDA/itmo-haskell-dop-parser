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

  KtPsiFun0
    :: (Console c)
    => Name
    -> KtAnyType
    -> [KotlinPsi (KtCommand c)]
    -> KotlinPsi (KtFunData (KtFun0 c))
  KtPsiFun1
    :: (Console c)
    => Name
    -> KtFunArg
    -> KtAnyType
    -> [KotlinPsi (KtCommand c)]
    -> KotlinPsi (KtFunData (KtFun1 c))
  KtPsiFun2
    :: (Console c)
    => Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [KotlinPsi (KtCommand c)]
    -> KotlinPsi (KtFunData (KtFun2 c))
  
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
  
  KtPsiCallFun0 :: (Console c) => Name -> KotlinPsi (KtValue c)
  KtPsiCallFun1 :: (Console c) => Name -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiCallFun2 :: (Console c) => Name -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  
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

instance Eq (KtDeclarations KotlinPsi c) where
  (==) :: KtDeclarations KotlinPsi c -> KtDeclarations KotlinPsi c -> Bool
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
  
  KtPsiCallFun0 nameL == KtPsiCallFun0 nameR = nameL == nameR
  KtPsiCallFun1 nameL aL == KtPsiCallFun1 nameR aR =
    (nameL == nameR)
      && (aL == aR)
  KtPsiCallFun2 nameL a1L a2L == KtPsiCallFun2 nameR a1R a2R =
    (nameL == nameR)
      && (a1L == a1R)
      && (a2L == a2R)

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
  KtPsiFile dec -> ktFile $ KtDeclarations
    { kdFun0 = transform <$> kdFun0 dec  
    , kdFun1 = transform <$> kdFun1 dec  
    , kdFun2 = transform <$> kdFun2 dec  
    }

  KtPsiFun0 n t cs     -> ktFun0 n t $ transform <$> cs
  KtPsiFun1 n a t cs   -> ktFun1 n a t $ transform <$> cs
  KtPsiFun2 n a b t cs -> ktFun2 n a b t $ transform <$> cs
  
  KtPsiInitVariable ic n t v -> ktInitVariable ic n t $ transform v
  KtPsiSetVariable n v       -> ktSetVariable n $ transform v

  KtPsiReturn r -> ktReturn $ transform r
  KtPsiValueCommand r -> ktValueCommand $ transform r
  
  KtPsiCallFun0 n -> ktCallFun0 n
  KtPsiCallFun1 n a -> ktCallFun1 n $ transform a
  KtPsiCallFun2 n a1 a2 -> ktCallFun2 n (transform a1) (transform a2)

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
