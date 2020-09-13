{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Parsing.Utils
  ( emptyProxyFunDec
  , putFun0Data
  , putFun1Data
  , putFun2Data
  , unproxyFunDec
  
  , checkedInt
  , checkedDouble
  , updatedString
  
  , defaultReturn
  ) where

import Data.List (intercalate)
import Data.Set  (Set, insert, member, empty)
import Text.Read (readMaybe)

import Kotlin.Dsl
import Kotlin.Utils
import Parsing.KotlinPsi
import Parsing.Result

data ProxyFunDec = ProxyFunDec
  { pfdUsedNames :: Set Name
  , pfdFun0      :: [KotlinPsi (KtFunData KtFun0)]
  , pfdFun1      :: [KotlinPsi (KtFunData KtFun1)]
  , pfdFun2      :: [KotlinPsi (KtFunData KtFun2)]
  }

emptyProxyFunDec :: ProxyFunDec
emptyProxyFunDec = ProxyFunDec
  { pfdUsedNames = empty
  , pfdFun0      = []
  , pfdFun1      = []
  , pfdFun2      = []
  }

checkAndThen 
  :: Name -> [KtAnyType] -> (ProxyFunDec -> ProxyFunDec) -> ProxyFunDec -> Result ProxyFunDec
checkAndThen name types updater proxy =
  let funKey = fun2key name types
    in if member name $ pfdUsedNames proxy
       then failE $
         "Multyply declaration of function: " ++
         name ++ "(" ++ intercalate ", " (show <$> types) ++ ")"
       else returnE . updater $ proxy { pfdUsedNames = insert name $ pfdUsedNames proxy }

putFun0Data :: KotlinPsi (KtFunData KtFun0) -> ProxyFunDec -> Result ProxyFunDec
putFun0Data f@(KtPsiFun0 name _ _) =
  checkAndThen name [] $ \proxy -> proxy { pfdFun0 = f : (pfdFun0 proxy) }

putFun1Data :: KotlinPsi (KtFunData KtFun1) -> ProxyFunDec -> Result ProxyFunDec
putFun1Data f@(KtPsiFun1 name (_, aType) _ _) =
  checkAndThen name [aType] $ \proxy -> proxy { pfdFun1 = f : (pfdFun1 proxy) }

putFun2Data :: KotlinPsi (KtFunData KtFun2) -> ProxyFunDec -> Result ProxyFunDec
putFun2Data f@(KtPsiFun2 name (_, a1Type) (_, a2Type) _ _) =
  checkAndThen name [a1Type, a2Type] $ \proxy -> proxy { pfdFun2 = f : (pfdFun2 proxy) }

unproxyFunDec :: ProxyFunDec -> KtDeclarations KotlinPsi
unproxyFunDec proxy = KtDeclarations
  { kdFun0 = pfdFun0 proxy
  , kdFun1 = pfdFun1 proxy
  , kdFun2 = pfdFun2 proxy
  }

checkedInt :: String -> Result (KotlinPsi KtAnyValue)
checkedInt str = case readMaybe @Int str of
  Nothing -> failE $ "Illegal Int constant: " ++ str
  Just i  -> returnE $ KtPsiInt i

checkedDouble :: String -> Result (KotlinPsi KtAnyValue)
checkedDouble str = case readMaybe @Double str of
  Nothing -> failE $ "Illegal Double constant: " ++ str
  Just d  -> returnE $ KtPsiDouble d

updatedString :: String -> KotlinPsi KtAnyValue
updatedString (_:str) = KtPsiString $ dropLast str

dropLast :: [a] -> [a]
dropLast xs = f xs (tail xs)
    where
      f :: [a] -> [a] -> [a] 
      f (x:xs) (y:ys) = x : f xs ys
      f _ _ = []

defaultReturn :: KotlinPsi KtCommand
defaultReturn = KtPsiReturn $ KtPsiUnit ()
