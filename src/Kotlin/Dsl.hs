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

  , KtAnyValue(..)
  , KtValue(..)
  , HiddenIO(..)

  , KtVariable(..)

  , Name(..)

  , KtType(..)
  , KtAnyType(..)
  ) where

import Data.Map      (Map)
import Data.Typeable (Typeable)

class Kotlin expr where
  ktFile :: KtDeclarations expr -> expr KtFile

  ktFun0
    :: Name
    -> KtAnyType
    -> [expr KtCommand]
    -> expr (KtFunData KtFun0)

  ktFun1
    :: Name
    -> KtFunArg
    -> KtAnyType
    -> [expr KtCommand]
    -> expr (KtFunData KtFun1)

  ktFun2
    :: Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [expr KtCommand]
    -> expr (KtFunData KtFun2)

  ktReturn :: expr KtAnyValue -> expr KtCommand
  
  ktValueCommand :: expr KtAnyValue -> expr KtCommand
  
  ktCallFun0 :: Name -> expr KtAnyValue
  
  ktCallFun1 :: Name -> expr KtAnyValue -> expr KtAnyValue
  
  ktCallFun2 :: Name -> expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue
  
  ktAddition :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktDifferent :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktMultiplication :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktRatio :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktNegate :: expr KtAnyValue -> expr KtAnyValue

  ktAnd :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktOr :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue
  
  ktNot :: expr KtAnyValue -> expr KtAnyValue

  ktEq :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue
  
  ktNotEq :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktGt :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktGte :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktLt :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktLte :: expr KtAnyValue -> expr KtAnyValue -> expr KtAnyValue

  ktInt    :: Int    -> expr KtAnyValue

  ktDouble :: Double -> expr KtAnyValue

  ktString :: String -> expr KtAnyValue

  ktBool   :: Bool   -> expr KtAnyValue

  ktUnit   :: ()     -> expr KtAnyValue

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
  KtFun0
    :: (Typeable r) 
    => (KtScope -> IO r) -> KtFun0

data KtFun1 where
  KtFun1
    :: (Typeable a, Typeable r)
    => (KtScope -> a -> IO r) -> KtFun1

data KtFun2 where
  KtFun2
    :: (Typeable a1, Typeable a2, Typeable r)
    => (KtScope -> a1 -> a2 -> IO r) -> KtFun2

type KtFunData fun = (Name, [KtAnyType], fun)

type Name = String

type KtFunArg = (Name, KtAnyType)

data KtCommand where
  KtCommandReturn :: KtAnyValue -> KtCommand
  KtCommandStep   :: (KtScope -> IO KtScope) -> KtCommand
  KtCommandBlock  :: [KtCommand] -> KtCommand

type KtAnyValue = KtScope -> HiddenIO
type KtValue a = KtScope -> IO a

data HiddenIO where
  HiddenIO :: (Typeable a) => IO a -> HiddenIO

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
