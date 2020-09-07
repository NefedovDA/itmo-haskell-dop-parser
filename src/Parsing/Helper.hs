{-# LANGUAGE GADTs #-}

module Parsing.Helper
  ( emptyProxyFunDec
  , putFun0Data
  , putFun1Data
  , putFun2Data
  , unproxyFunDec
  ) where

import Data.Set (Set, insert, member, empty)

import Kotlin.Dsl
import Parsing.KotlinPsi
import Parsing.Result

data ProxyFunDec = ProxyFunDec
  { pfdUsedNames :: Set Name
  , pfdFun0      :: [KotlinPsi KtFun0Data]
  , pfdFun1      :: [KotlinPsi KtFun1Data]
  , pfdFun2      :: [KotlinPsi KtFun2Data]
  }

emptyProxyFunDec :: ProxyFunDec
emptyProxyFunDec = ProxyFunDec
  { pfdUsedNames = empty
  , pfdFun0      = []
  , pfdFun1      = []
  , pfdFun2      = []
  }

checkAndThen :: Name -> (ProxyFunDec -> ProxyFunDec) -> ProxyFunDec -> Result ProxyFunDec
checkAndThen name updater proxy =
  if member name $ pfdUsedNames proxy
  then failE $ "Name `" ++ name ++ "` alrady used"
  else returnE . updater $ proxy { pfdUsedNames = insert name $ pfdUsedNames proxy }

putFun0Data :: KotlinPsi KtFun0Data -> ProxyFunDec -> Result ProxyFunDec
putFun0Data f@(KtPsiFun0 name _) =
  checkAndThen name $ \proxy -> proxy { pfdFun0 = f : (pfdFun0 proxy) }

putFun1Data :: KotlinPsi KtFun1Data -> ProxyFunDec -> Result ProxyFunDec
putFun1Data f@(KtPsiFun1 name _ _) =
  checkAndThen name $ \proxy -> proxy { pfdFun1 = f : (pfdFun1 proxy) }

putFun2Data :: KotlinPsi KtFun2Data -> ProxyFunDec -> Result ProxyFunDec
putFun2Data f@(KtPsiFun2 name _ _ _) =
  checkAndThen name $ \proxy -> proxy { pfdFun2 = f : (pfdFun2 proxy) }

unproxyFunDec :: ProxyFunDec -> FunDecl KotlinPsi
unproxyFunDec proxy = FunDecl
  { fdFun0 = pfdFun0 proxy
  , fdFun1 = pfdFun1 proxy
  , fdFun2 = pfdFun2 proxy
  }