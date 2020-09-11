{-# LANGUAGE GADTs #-}

module Kotlin.Dsl
  ( Kotlin(..)

  , KtFile(..)
  
  , KtDeclarations(..)
  , KtScope(..)

  , KtFun0(..)
  , KtFun1(..)
  , KtFun2(..)
  
  , KtFunData(..)

  , KtFunArg(..)
  
  , KtCommand(..)

  , KtValue(..)
  , KtVariable(..)

  , Name(..)

  , KtType(..)
  , KtAnyType(..)
  ) where

import Data.Map      (Map)
import Data.Typeable (Typeable)

class Kotlin expr where
  ktFile :: KtDeclarations expr -> expr KtFile

  ktFun0 :: Name -> KtAnyType -> [expr KtCommand] -> expr (KtFunData KtFun0)
  ktFun1 :: Name -> KtFunArg -> KtAnyType -> [expr KtCommand] -> expr (KtFunData KtFun1)
  ktFun2 :: Name -> KtFunArg -> KtFunArg -> KtAnyType -> [expr KtCommand] -> expr (KtFunData KtFun2)

  ktReturn :: expr KtValue -> expr KtCommand

  ktInt    :: Int    -> expr KtValue
  ktDouble :: Double -> expr KtValue
  ktString :: String -> expr KtValue
  ktBool   :: Bool   -> expr KtValue
  ktUnit   :: ()     -> expr KtValue

type KtFile = IO ()

data KtDeclarations expr = KtDeclarations
  { kdFun0 :: [expr (KtFunData KtFun0)]
  , kdFun1 :: [expr (KtFunData KtFun1)]
  , kdFun2 :: [expr (KtFunData KtFun2)]
  }

data KtScope = KtScope
  { sFun0 :: Map String KtFun0
  , sFun1 :: Map String KtFun1
  , sFun2 :: Map String KtFun2
  
  , sValue :: Map String KtVariable
  , sVariable :: Map String KtVariable 
  }

data KtFun0 where
  KtFun0 :: (Typeable r) => (KtScope -> IO r) -> KtFun0

data KtFun1 where
  KtFun1 :: (Typeable a, Typeable r) => (KtScope -> a -> IO r) -> KtFun1

data KtFun2 where
  KtFun2 :: (Typeable a1, Typeable a2, Typeable r) => (KtScope -> a1 -> a2 -> IO r) -> KtFun2

type KtFunData fun = (Name, [KtAnyType], fun)

type Name = String

type KtFunArg = (Name, KtAnyType)

data KtCommand where
  KtCommandReturn :: KtValue -> KtCommand
  KtCommandStep   :: (KtScope -> IO (KtScope, ())) -> KtCommand
  KtCommandBlock  :: [KtCommand] -> KtCommand

data KtValue where
  KtValue :: (Typeable a) => (KtScope -> IO a) -> KtValue

data KtVariable where
  KtVariable :: (Typeable a) => KtType a -> a -> KtVariable

data KtType t where
  KtIntType    :: KtType Int
  KtDoubleType :: KtType Double
  KtStringType :: KtType String
  KtUnitType   :: KtType ()
  KtBoolType   :: KtType Bool

data KtAnyType where
  KtAnyType :: (Typeable t) => KtType t -> KtAnyType
