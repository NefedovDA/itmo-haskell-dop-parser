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
  KtPsiFile fDecL == KtPsiFile fDecR = fDecL == fDecR
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

  KtPsiInt i    -> ktInt i   
  KtPsiDouble d -> ktDouble d
  KtPsiString s -> ktString s
  KtPsiBool b   -> ktBool b  
  KtPsiUnit ()  -> ktUnit ()  
