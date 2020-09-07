{-# LANGUAGE GADTs #-}

module Parsing.KotlinPsi
  ( KotlinPsi(..)
  , KtType(..)
  
  , transform
  ) where

import Kotlin.Dsl

data KotlinPsi a where
  KtPsiFile :: FunDecl KotlinPsi -> KotlinPsi KtFile
  KtPsiFun0 :: Name -> KtType -> KotlinPsi KtFun0Data
  KtPsiFun1 :: Name -> KtFunArg -> KtType -> KotlinPsi KtFun1Data
  KtPsiFun2 :: Name -> KtFunArg -> KtFunArg -> KtType -> KotlinPsi KtFun2Data

transform :: Kotlin expr => KotlinPsi a -> expr a
transform a = case a of
  KtPsiFile fDec -> ktFile $ FunDecl
    { fdFun0 = transform <$> fdFun0 fDec
    , fdFun1 = transform <$> fdFun1 fDec
    , fdFun2 = transform <$> fdFun2 fDec
    }
  KtPsiFun0 n t      -> ktFun0 n t
  KtPsiFun1 n a t    -> ktFun1 n a t
  KtPsiFun2 n a b t  -> ktFun2 n a b t
