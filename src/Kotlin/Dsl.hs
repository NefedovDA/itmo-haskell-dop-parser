{-# LANGUAGE GADTs #-}

module Kotlin.Dsl
  ( Kotlin(..)
  
  , KtDeclarations(..)
  , KtScope(..)

  , KtFile

  , KtFun0
  , KtFun1
  , KtFun2
  
  , KtFunData

  , KtFunArg
  
  , KtValue

  , Name

  , KtCommand(..)

  , HiddenIO(..)

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

  ktReturn :: expr KtValue -> expr KtCommand

  ktValueCommand :: expr KtValue -> expr KtCommand

  ktCallFun0 :: Name -> expr KtValue

  ktCallFun1 :: Name -> expr KtValue -> expr KtValue

  ktCallFun2 :: Name -> expr KtValue -> expr KtValue -> expr KtValue

  ktAddition :: expr KtValue -> expr KtValue -> expr KtValue

  ktDifferent :: expr KtValue -> expr KtValue -> expr KtValue

  ktMultiplication :: expr KtValue -> expr KtValue -> expr KtValue

  ktRatio :: expr KtValue -> expr KtValue -> expr KtValue

  ktNegate :: expr KtValue -> expr KtValue

  ktAnd :: expr KtValue -> expr KtValue -> expr KtValue

  ktOr :: expr KtValue -> expr KtValue -> expr KtValue

  ktNot :: expr KtValue -> expr KtValue

  ktEq :: expr KtValue -> expr KtValue -> expr KtValue

  ktNotEq :: expr KtValue -> expr KtValue -> expr KtValue

  ktGt :: expr KtValue -> expr KtValue -> expr KtValue

  ktGte :: expr KtValue -> expr KtValue -> expr KtValue

  ktLt :: expr KtValue -> expr KtValue -> expr KtValue

  ktLte :: expr KtValue -> expr KtValue -> expr KtValue

  ktInt    :: Int -> expr KtValue

  ktDouble :: Double -> expr KtValue

  ktString :: String -> expr KtValue

  ktBool   :: Bool -> expr KtValue

  ktUnit   :: () -> expr KtValue

data KtDeclarations expr = KtDeclarations
  { kdFun0 :: [expr (KtFunData KtFun0)]
  , kdFun1 :: [expr (KtFunData KtFun1)]
  , kdFun2 :: [expr (KtFunData KtFun2)]
  }

data KtScope = KtScope
  { sFun0 :: Map String KtFun0
  , sFun1 :: Map String KtFun1
  , sFun2 :: Map String KtFun2
  
  , sValue :: Map String HiddenIO
  , sVariable :: Map String HiddenIO
  }

type KtFile = IO ()

type KtFun0 = KtScope -> HiddenIO

type KtFun1 = KtScope -> HiddenIO -> HiddenIO

type KtFun2 = KtScope -> HiddenIO -> HiddenIO -> HiddenIO

type KtFunData fun = (Name, fun)

type Name = String

type KtFunArg = (Name, KtAnyType)

type KtValue = KtScope -> HiddenIO

data KtCommand where
  KtCommandReturn :: KtValue -> KtCommand
  KtCommandStep   :: (KtScope -> IO KtScope) -> KtCommand
  KtCommandBlock  :: [KtCommand] -> KtCommand

data HiddenIO where
  HiddenIO :: (Typeable a) => KtType a -> IO a -> HiddenIO

data KtType t where
  KtIntType    :: KtType Int
  KtDoubleType :: KtType Double
  KtStringType :: KtType String
  KtUnitType   :: KtType ()
  KtBoolType   :: KtType Bool

data KtAnyType where
  KtAnyType :: (Typeable t) => KtType t -> KtAnyType
