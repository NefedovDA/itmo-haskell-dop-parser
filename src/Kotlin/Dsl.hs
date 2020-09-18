{-# LANGUAGE GADTs #-}

module Kotlin.Dsl
  ( Kotlin(..)
  , Console(..)
  
  , KtDeclarations(..)
  , KtScope(..)

  , KtFile

  , KtFun0
  , KtFun1
  , KtFun2
  
  , KtFunData

  , KtVariableInfo

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
  ktFile :: (Console c) => KtDeclarations expr c -> expr (KtFile c)

  ktFun0
    :: (Console c)
    => Name
    -> KtAnyType
    -> [expr (KtCommand c)]
    -> expr (KtFunData (KtFun0 c))

  ktFun1
    :: (Console c)
    => Name
    -> KtFunArg
    -> KtAnyType
    -> [expr (KtCommand c)]
    -> expr (KtFunData (KtFun1 c))

  ktFun2
    :: (Console c)
    => Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [expr (KtCommand c)]
    -> expr (KtFunData (KtFun2 c))

  ktInitVariable
    :: (Console c)
    => Bool
    -> Name
    -> KtAnyType
    -> expr (KtValue c)
    -> expr (KtCommand c)

  ktSetVariable :: (Console c) => Name -> expr (KtValue c) -> expr (KtCommand c)

  ktReturn :: (Console c) => expr (KtValue c) -> expr (KtCommand c)

  ktValueCommand :: (Console c) => expr (KtValue c) -> expr (KtCommand c)

  ktCallFun0 :: (Console c) => Name -> expr (KtValue c)

  ktCallFun1 :: (Console c) => Name -> expr (KtValue c) -> expr (KtValue c)

  ktCallFun2 :: (Console c) => Name -> expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktReadVariable :: (Console c) => Name -> expr (KtValue c)

  ktFor
    :: (Console c)
    => Name
    -> expr (KtValue c)
    -> expr (KtValue c)
    -> [expr (KtCommand c)]
    -> expr (KtCommand c)

  ktIf
    :: (Console c)
    => [(expr (KtValue c), [expr (KtCommand c)])]
    -> [expr (KtCommand c)]
    -> expr (KtCommand c)

  ktAddition :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktDifferent :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktMultiplication :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktRatio :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktNegate :: (Console c) => expr (KtValue c) -> expr (KtValue c)

  ktAnd :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktOr :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktNot :: (Console c) => expr (KtValue c) -> expr (KtValue c)

  ktEq :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktNotEq :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktGt :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktGte :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktLt :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktLte :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktInt :: (Console c) => Int -> expr (KtValue c)

  ktDouble :: (Console c) => Double -> expr (KtValue c)

  ktString :: (Console c) => String -> expr (KtValue c)

  ktBool :: (Console c) => Bool -> expr (KtValue c)

  ktUnit :: (Console c) => () -> expr (KtValue c)

class Monad m => Console m where
  consolePrint    :: String -> m ()
  consolePrintln  :: String -> m ()
  consoleReadLine :: m String

data KtDeclarations expr c = KtDeclarations
  { kdFun0 :: [expr (KtFunData (KtFun0 c))]
  , kdFun1 :: [expr (KtFunData (KtFun1 c))]
  , kdFun2 :: [expr (KtFunData (KtFun2 c))]
  }

data KtScope c = KtScope
  { sFun0 :: Map String (KtFun0 c)
  , sFun1 :: Map String (KtFun1 c)
  , sFun2 :: Map String (KtFun2 c)
  
  , sVariable :: [Map String (KtVariableInfo c)]
  }

type KtVariableInfo c = (Bool, HiddenIO c)

type KtFile c = c ()

type KtFun0 c = KtScope c -> HiddenIO c

type KtFun1 c = KtScope c -> HiddenIO c -> HiddenIO c

type KtFun2 c = KtScope c -> HiddenIO c -> HiddenIO c -> HiddenIO c

type KtFunData fun = (Name, fun)

type Name = String

type KtFunArg = (Name, KtAnyType)

type KtValue c = KtScope c -> HiddenIO c

data KtCommand c where
  KtCmdReturn
    :: KtValue c
    -> KtCommand c
  KtCmdStep
    :: (KtScope c -> c (KtScope c))
    -> KtCommand c
  KtCmdFor
    :: Name
    -> [KtCommand c]
    -> (KtScope c -> c (Int, Int))
    -> KtCommand c
  KtCmdIf
    :: [(KtScope c -> c Bool, [KtCommand c])]
    -> KtCommand c

data HiddenIO c where
  HiddenIO :: (Typeable a) => KtType a -> c a -> HiddenIO c

data KtType t where
  KtIntType    :: KtType Int
  KtDoubleType :: KtType Double
  KtStringType :: KtType String
  KtUnitType   :: KtType ()
  KtBoolType   :: KtType Bool

data KtAnyType where
  KtAnyType :: (Typeable t) => KtType t -> KtAnyType
