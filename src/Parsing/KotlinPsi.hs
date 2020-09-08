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

import Kotlin.Dsl
import Data.Typeable ((:~:)(..), eqT)

import Kotlin.Printer (runPrint)

data KotlinPsi a where
  KtPsiFile :: FunDecl KotlinPsi -> KotlinPsi KtFile
  KtPsiFun0 :: Name -> KtType -> KotlinPsi KtFun0Data
  KtPsiFun1 :: Name -> KtFunArg -> KtType -> KotlinPsi KtFun1Data
  KtPsiFun2 :: Name -> KtFunArg -> KtFunArg -> KtType -> KotlinPsi KtFun2Data
  KtPsiReturn :: KtHiddenResult KotlinPsi -> KotlinPsi KtCmd
  KtPsiInt    :: Int -> KotlinPsi KtInt
  KtPsiDouble :: Double -> KotlinPsi KtDouble
  KtPsiString :: String -> KotlinPsi KtString
  KtPsiBool   :: Bool -> KotlinPsi KtBool
  KtPsiUnit   :: () -> KotlinPsi KtUnit

instance Eq (FunDecl KotlinPsi) where
  (==) :: FunDecl KotlinPsi -> FunDecl KotlinPsi -> Bool
  fDecL == fDecR = 
    (fdFun0 fDecL == fdFun0 fDecR) && 
    (fdFun1 fDecL == fdFun1 fDecR) && 
    (fdFun2 fDecL == fdFun2 fDecR)

instance Eq (KotlinPsi a) where
  (==) :: KotlinPsi a -> KotlinPsi a -> Bool
  KtPsiFile fDecL == KtPsiFile fDecR = fDecL == fDecR
  KtPsiFun0 nameL rTypeL == KtPsiFun0 nameR rTypeR =
    (nameL  == nameR ) &&
    (rTypeL == rTypeR)
  KtPsiFun1 nameL argL rTypeL == KtPsiFun1 nameR argR rTypeR =
    (nameL  == nameR ) &&
    (argL   == argR  ) &&
    (rTypeL == rTypeR)
  KtPsiFun2 nameL arg1L arg2L rTypeL == KtPsiFun2 nameR arg1R arg2R rTypeR =
    (nameL  == nameR ) &&
    (arg1L  == arg1R ) &&
    (arg2L  == arg2R ) &&
    (rTypeL == rTypeR)
  (==) (KtPsiReturn (KtHiddenResult (rL :: KotlinPsi aL)))
       (KtPsiReturn (KtHiddenResult (rR :: KotlinPsi aR))) =
    case eqT @aL @aR of
      Nothing   -> False
      Just Refl -> rL == rR
  KtPsiInt iL == KtPsiInt iR = iL == iR
  KtPsiDouble dL == KtPsiDouble dR = dL == dR
  KtPsiString sL == KtPsiString sR = sL == sR
  KtPsiBool bL == KtPsiBool bR = bL == bR
  KtPsiUnit () == KtPsiUnit () = True
  _ == _ = False

instance Show (KotlinPsi a) where
  show :: KotlinPsi a -> String
  show = runPrint . transform

transformHiddenResult :: Kotlin expr => KtHiddenResult KotlinPsi -> KtHiddenResult expr
transformHiddenResult (KtHiddenResult r) = KtHiddenResult $ transform r 

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
  KtPsiReturn r -> ktReturn $ transformHiddenResult r
  KtPsiInt i    -> ktInt i   
  KtPsiDouble d -> ktDouble d
  KtPsiString s -> ktString s
  KtPsiBool b   -> ktBool b  
  KtPsiUnit ()  -> ktUnit ()  
