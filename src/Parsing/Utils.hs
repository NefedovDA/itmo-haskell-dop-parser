{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

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
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Kotlin.Dsl
import Kotlin.Utils
import Parsing.KotlinPsi
import Parsing.Result

data ProxyFunDec = ProxyFunDec
  { pfdUsedNames    :: Set Name
  , pfdDeclarations :: KtDeclarations KotlinPsi IO
  }

emptyProxyFunDec :: ProxyFunDec
emptyProxyFunDec = ProxyFunDec
  { pfdUsedNames = empty
  , pfdDeclarations = KtDeclarations
      { kdFun0      = []
      , kdFun1      = []
      , kdFun2      = []
      }
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

putFun0Data :: KotlinPsi (KtFunData (KtFun0 IO)) -> ProxyFunDec -> Result ProxyFunDec
putFun0Data f@(KtPsiFun0 name _ _) =
  checkAndThen name [] $ \proxy -> proxy 
    { pfdDeclarations = (pfdDeclarations proxy)
        { kdFun0 = f : (kdFun0 $ pfdDeclarations proxy) }
    }

putFun1Data :: KotlinPsi (KtFunData (KtFun1 IO)) -> ProxyFunDec -> Result ProxyFunDec
putFun1Data f@(KtPsiFun1 name (_, aType) _ _) =
  checkAndThen name [aType] $ \proxy -> proxy 
    { pfdDeclarations = (pfdDeclarations proxy)
        { kdFun1 = f : (kdFun1 $ pfdDeclarations proxy) }
    }

putFun2Data :: KotlinPsi (KtFunData (KtFun2 IO)) -> ProxyFunDec -> Result ProxyFunDec
putFun2Data f@(KtPsiFun2 name (_, a1Type) (_, a2Type) _ _) =
  checkAndThen name [a1Type, a2Type] $ \proxy -> proxy 
    { pfdDeclarations = (pfdDeclarations proxy)
        { kdFun2 = f : (kdFun2 $ pfdDeclarations proxy) }
    }

unproxyFunDec :: ProxyFunDec -> KtDeclarations KotlinPsi IO
unproxyFunDec = pfdDeclarations

checkedInt :: String -> Result (KotlinPsi (KtValue IO))
checkedInt str = case readMaybe @Int str of
  Nothing -> failE $ "Illegal Int constant: " ++ str
  Just i  -> returnE $ KtPsiInt i

checkedDouble :: String -> Result (KotlinPsi (KtValue IO))
checkedDouble str = case readMaybe @Double str of
  Nothing -> failE $ "Illegal Double constant: " ++ str
  Just d  -> returnE $ KtPsiDouble d

updatedString :: String -> KotlinPsi (KtValue IO)
updatedString (_:str) = KtPsiString $ dropLast str

dropLast :: [a] -> [a]
dropLast xs = f xs (tail xs)
    where
      f :: [a] -> [a] -> [a] 
      f (x:xs) (y:ys) = x : f xs ys
      f _ _ = []

defaultReturn :: KotlinPsi (KtCommand IO)
defaultReturn = KtPsiReturn $ KtPsiUnit ()

instance Console IO where
  consolePrint :: String -> IO ()
  consolePrint s = putStr s >> hFlush stdout
  
  consolePrintln  :: String -> IO ()
  consolePrintln = putStrLn
  
  consoleReadLine :: IO String
  consoleReadLine = readLn