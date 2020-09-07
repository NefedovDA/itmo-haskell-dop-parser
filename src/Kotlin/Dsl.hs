{-# LANGUAGE GADTs                      #-}

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

  , Name(..)
  , Unit(..)
  
  , KtType(..)
  ) where

import Data.Map      (Map)
import Data.Typeable (Typeable)

class Kotlin expr where
  ktFile :: FunDecl expr -> expr KtFile
  ktFun0 :: Name -> KtType -> expr KtFun0Data
  ktFun1 :: Name -> KtFunArg -> KtType -> expr KtFun1Data
  ktFun2 :: Name -> KtFunArg -> KtFunArg -> KtType -> expr KtFun2Data

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

type KtFun0Data = (Name, KtFun0)

type KtFun1Data = (Name, KtFun1)

type KtFun2Data = (Name, KtFun2)

type KtFunArg = (Name, KtType)

type Name = String

type Unit = ()

data KtType
  = KtInt
  | KtDouble
  | KtString
  | KtUnit
  | KtBool
