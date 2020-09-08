{-# LANGUAGE GADTs #-}

module Kotlin.Dsl
  ( Kotlin(..)

  , FunDecl(..)
  , Scope(..)

  , KtFile(..)

  , KtFun0Data(..)
  , KtFun1Data(..)
  , KtFun2Data(..)

  , KtFun0(..)
  , KtFun1(..)
  , KtFun2(..)

  , KtFunArg(..)
  
  , KtCmd(..)

  , KtBool(..)
  , KtDouble(..)
  , KtInt(..)
  , KtString(..)
  , KtUnit(..)
  , KtHiddenResult(..)

  , Name(..)

  , KtType(..)
  ) where

import Data.Map      (Map)
import Data.Typeable (Typeable)

class Kotlin expr where
  ktFile :: FunDecl expr -> expr KtFile
  ktFun0 :: Name -> KtType -> expr KtFun0Data
  ktFun1 :: Name -> KtFunArg -> KtType -> expr KtFun1Data
  ktFun2 :: Name -> KtFunArg -> KtFunArg -> KtType -> expr KtFun2Data
  ktReturn :: KtHiddenResult expr -> expr KtCmd
  ktInt    :: Int    -> expr KtInt
  ktDouble :: Double -> expr KtDouble
  ktString :: String -> expr KtString
  ktBool   :: Bool   -> expr KtBool
  ktUnit   :: ()     -> expr KtUnit

data FunDecl expr = FunDecl
  { fdFun0 :: [expr KtFun0Data]
  , fdFun1 :: [expr KtFun1Data]
  , fdFun2 :: [expr KtFun2Data]
  }

type KtFile = IO ()

data Scope = Scope
  { sFun0 :: Map Name KtFun0
  , sFun1 :: Map Name KtFun1
  , sFun2 :: Map Name KtFun2
  }

data KtFun0 where
  KtFun0 :: (Typeable r) => (Scope -> IO r) -> KtFun0

data KtFun1 where
  KtFun1 :: (Typeable a1, Typeable r) => (Scope -> a1 -> IO r) -> KtFun1

data KtFun2 where
  KtFun2 :: (Typeable a1, Typeable a2, Typeable r) => (Scope -> a1 -> a2 -> IO r) -> KtFun2

type Name = String

type KtFun0Data = (Name, KtFun0)

type KtFun1Data = (Name, KtFun1)

type KtFun2Data = (Name, KtFun2)

type KtFunArg = (Name, KtType)

data KtCmd where
  KtCmdStep   :: (Scope -> (IO a, Scope)) -> KtCmd
  KtCmdBlock  :: (Scope -> [KtCmd]) -> KtCmd
  KtCmdReturn :: (Scope -> IO a) -> KtCmd

type KtInt = IO Int

type KtDouble = IO Double

type KtString = IO String

type KtUnit = IO ()

type KtBool = IO Bool

data KtHiddenResult expr where
 KtHiddenResult :: Typeable a => expr (IO a) -> KtHiddenResult expr

data KtType
  = KtIntType
  | KtDoubleType
  | KtStringType
  | KtUnitType
  | KtBoolType
  deriving (Eq)
