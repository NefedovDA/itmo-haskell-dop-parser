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
import Data.Typeable  (eqT)

import Kotlin.Dsl
import Kotlin.Printer (Printer)

infixl 7 :*:, :/:
infixl 6 :+:, :-:
infixl 5 :>:, :>=:, :<:, :<=:
infixl 4 :==:, :!=:
infixl 3 :&&:
infixl 2 :||:

-- | Proxy data type for use it in Happy.
-- Has the same construction as 'Kotlin'.
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

  KtPsiCallFun :: (Console c) => Name -> [KotlinPsi (KtValue c)] -> KotlinPsi (KtValue c)
  KtPsiReadVariable :: (Console c) => Name -> KotlinPsi (KtValue c)

  KtPsiNegate :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  KtPsiNot    :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)

  (:*:)  :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:/:)  :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:+:)  :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:-:)  :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:>:)  :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:>=:) :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:<:)  :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:<=:) :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:==:) :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:!=:) :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:&&:) :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)
  (:||:) :: (Console c) => KotlinPsi (KtValue c) -> KotlinPsi (KtValue c) -> KotlinPsi (KtValue c)

  KtPsiInt    :: (Console c) => Int    -> KotlinPsi (KtValue c)
  KtPsiDouble :: (Console c) => Double -> KotlinPsi (KtValue c)
  KtPsiString :: (Console c) => String -> KotlinPsi (KtValue c)
  KtPsiBool   :: (Console c) => Bool   -> KotlinPsi (KtValue c)
  KtPsiUnit   :: (Console c) => ()     -> KotlinPsi (KtValue c)

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
    (nameL == nameR) && (valueL == valueL)
  KtPsiReturn valueL == KtPsiReturn valueR = valueL == valueR
  KtPsiValueCommand valueL == KtPsiValueCommand valueR = valueL == valueR
  KtPsiCallFun nameL argsL == KtPsiCallFun nameR argsR =
    (nameL == nameR) && (argsL == argsR)
  KtPsiReadVariable nameL == KtPsiReadVariable nameR = nameL == nameR
  KtPsiFor nameL fromL toL cmdsL == KtPsiFor nameR fromR toR cmdsR =
    (nameL == nameR)
      && (fromL == fromR)
      && (toL == toR)
      && (cmdsR == cmdsR)
  KtPsiIf branchesL elseBranchL == KtPsiIf branchesR elseBranchR =
    (branchesL == branchesR) && (branchesL == branchesR)

  KtPsiNegate vL == KtPsiNegate vR = vL == vR
  KtPsiNot vL    == KtPsiNot vR    = vL == vR

  (lvL :*: rvL)  == (lvR :*: rvR)  = (lvL == lvR) && (rvL == rvR)
  (lvL :/: rvL)  == (lvR :/: rvR)  = (lvL == lvR) && (rvL == rvR)
  (lvL :+: rvL)  == (lvR :+: rvR)  = (lvL == lvR) && (rvL == rvL)
  (lvL :-: rvL)  == (lvR :-: rvR)  = (lvL == lvR) && (rvL == rvR)
  (lvL :>: rvL)  == (lvR :>: rvR)  = (lvL == lvR) && (rvL == rvR)
  (lvL :>=: rvL) == (lvR :>=: rvR) = (lvL == lvR) && (rvL == rvR)
  (lvL :<: rvL)  == (lvR :<: rvR)  = (lvL == lvR) && (rvL == rvR)
  (lvL :<=: rvL) == (lvR :<=: rvR) = (lvL == lvR) && (rvL == rvR)
  (lvL :==: rvL) == (lvR :==: rvR) = (lvL == lvR) && (rvL == rvR)
  (lvL :!=: rvL) == (lvR :!=: rvR) = (lvL == lvR) && (rvL == rvR)
  (lvL :&&: rvL) == (lvR :&&: rvR) = (lvL == lvR) && (rvL == rvR)
  (lvL :||: rvL) == (lvR :||: rvR) = (lvL == lvR) && (rvL == rvR)

  KtPsiInt iL == KtPsiInt iR = iL == iR
  KtPsiDouble dL == KtPsiDouble dR = dL == dR
  KtPsiString sL == KtPsiString sR = sL == sR
  KtPsiBool bL == KtPsiBool bR = bL == bR
  KtPsiUnit () == KtPsiUnit () = True

  _ == _ = False

instance Show (KotlinPsi a) where
  show :: KotlinPsi a -> String
  show = show . transform @Printer

-- | Transform KotlinPsi to the any Kotlin instance.
transform :: Kotlin expr => KotlinPsi a -> expr a
transform a = case a of
  KtPsiFile ds -> ktFile $ transform <$> ds

  KtPsiFun n as t cs   -> ktFun n as t $ transform <$> cs

  KtPsiInitVariable ic n t v -> ktInitVariable ic n t $ transform v
  KtPsiSetVariable n v       -> ktSetVariable n $ transform v
  KtPsiReturn r -> ktReturn $ transform r
  KtPsiValueCommand r -> ktValueCommand $ transform r
  KtPsiFor n f t cs -> ktFor n (transform f) (transform t) (transform <$> cs)
  KtPsiIf bs eb -> ktIf (first transform . second (transform <$>) <$> bs) (transform <$> eb)

  KtPsiCallFun n as -> ktCallFun n $ transform <$> as
  KtPsiReadVariable n  -> ktReadVariable n

  KtPsiNot v    -> ktNot $ transform v
  KtPsiNegate v -> ktNegate $ transform v

  lv :*:  rv -> (transform lv) @*@  (transform rv)
  lv :/:  rv -> (transform lv) @/@  (transform rv)
  lv :+:  rv -> (transform lv) @+@  (transform rv)
  lv :-:  rv -> (transform lv) @-@  (transform rv)
  lv :>:  rv -> (transform lv) @>@  (transform rv)
  lv :>=: rv -> (transform lv) @>=@ (transform rv)
  lv :<:  rv -> (transform lv) @<@  (transform rv)
  lv :<=: rv -> (transform lv) @<=@ (transform rv)
  lv :==: rv -> (transform lv) @==@ (transform rv)
  lv :!=: rv -> (transform lv) @!=@ (transform rv)
  lv :&&: rv -> (transform lv) @&&@ (transform rv)
  lv :||: rv -> (transform lv) @||@ (transform rv)

  KtPsiInt i    -> ktInt i
  KtPsiDouble d -> ktDouble d
  KtPsiString s -> ktString s
  KtPsiBool b   -> ktBool b
  KtPsiUnit ()  -> ktUnit ()
