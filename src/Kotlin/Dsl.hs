{-# LANGUAGE GADTs #-}

module Kotlin.Dsl
  ( Kotlin(..)
  , Console(..)

  , KtDeclarations
  , KtScope(..)

  , KtFile

  , KtFun

  , KtFunArg
  , KtFunData
  , KtFunKey

  , KtVariableInfo

  , KtValue

  , Name

  , KtCommand(..)

  , HiddenIO(..)

  , KtAnyType(..)
  , KtType(..)
  ) where

import Data.Map      (Map)
import Data.Typeable (Typeable)

class Kotlin expr where
  ktFile :: (Console c) => KtDeclarations expr c -> expr (KtFile c)

  ktFun
    :: (Console c)
    => Name
    -> [KtFunArg]
    -> KtAnyType
    -> [expr (KtCommand c)]
    -> expr (KtFunData c)

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

  ktCallFun :: (Console c) => Name -> [expr (KtValue c)] -> expr (KtValue c)

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

  ktNegate :: (Console c) => expr (KtValue c) -> expr (KtValue c)

  ktNot :: (Console c) => expr (KtValue c) -> expr (KtValue c)

  infixl 7 @*@
  (@*@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 7 @/@
  (@/@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 6 @+@
  (@+@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 6 @-@
  (@-@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 5 @>@
  (@>@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 5 @>=@
  (@>=@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 5 @<@
  (@<@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 5 @<=@
  (@<=@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 4 @==@
  (@==@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 4 @!=@
  (@!=@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 3 @&&@
  (@&&@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  infixl 2 @||@
  (@||@) :: (Console c) => expr (KtValue c) -> expr (KtValue c) -> expr (KtValue c)

  ktInt :: (Console c) => Int -> expr (KtValue c)

  ktDouble :: (Console c) => Double -> expr (KtValue c)

  ktString :: (Console c) => String -> expr (KtValue c)

  ktBool :: (Console c) => Bool -> expr (KtValue c)

  ktUnit :: (Console c) => () -> expr (KtValue c)

class Monad c => Console c where
  consolePrint    :: String -> c ()
  consolePrintln  :: String -> c ()
  consoleReadLine :: c String

type KtDeclarations expr c = [expr (KtFunData c)]

data KtScope c = KtScope
  { sFun      :: Map KtFunKey (KtFun c)
  , sVariable :: [Map String (KtVariableInfo c)]
  }

type KtVariableInfo c = (Bool, HiddenIO c)

type KtFile c = c ()

type KtFun c = KtScope c -> [HiddenIO c] -> HiddenIO c

type KtFunData c = (KtFunKey, KtFun c)

type KtFunKey = (Name, [KtAnyType])

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
