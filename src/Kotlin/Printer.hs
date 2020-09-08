{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}

module Kotlin.Printer
  ( Printer (..)
  ) where

import Data.List (intercalate)

import Kotlin.Dsl

newtype Printer a = Printer { runPrint :: String }
  deriving (Show, Semigroup)

castP :: Printer a -> Printer b
castP (Printer s) = Printer s

instance Kotlin Printer where
  ktFile :: FunDecl Printer -> Printer KtFile
  ktFile fnDecl = Printer $
    (intercalate "\n" $ runPrint <$> fdFun0 fnDecl) ++
    (intercalate "\n" $ runPrint <$> fdFun1 fnDecl) ++
    (intercalate "\n" $ runPrint <$> fdFun2 fnDecl)

  ktFun0 :: Name -> KtType -> Printer KtFun0Data
  ktFun0 name rType = printKtFun name [] rType

  ktFun1 :: Name -> KtFunArg -> KtType -> Printer KtFun1Data
  ktFun1 name arg rType = printKtFun name [arg] rType

  ktFun2 :: Name -> KtFunArg -> KtFunArg -> KtType -> Printer KtFun2Data
  ktFun2 name arg1 arg2 rType = printKtFun name [arg1, arg2] rType
  
  ktReturn :: KtHiddenResult Printer -> Printer KtCmd
  ktReturn (KtHiddenResult r) = (Printer "return ") <> castP r <> (Printer ";")
  
  ktInt :: Int -> Printer KtInt
  ktInt = Printer . show
  
  ktDouble :: Double -> Printer KtDouble
  ktDouble = Printer . show
  
  ktString :: String -> Printer KtString
  ktString s = Printer $ "\"" ++ s ++  "\""
  
  ktBool :: Bool -> Printer KtBool
  ktBool b = Printer $ if b then "true" else "false"
  
  ktUnit :: () -> Printer KtUnit
  ktUnit () = Printer $ "Unit"

instance Show KtType where
  show :: KtType -> String
  show = \case
    KtIntType    -> "Int"
    KtDoubleType -> "Double"
    KtStringType -> "String"
    KtUnitType   -> "Unit"
    KtBoolType   -> "Bool"

showKtArg :: KtFunArg -> String
showKtArg (name, aType) = name ++ ": " ++ show aType

printKtFun :: Name -> [KtFunArg] -> KtType -> Printer a
printKtFun name args rType = Printer $
  "fun " ++ name ++ "(" ++ (intercalate ", " $ showKtArg <$> args) ++  "): " ++ show rType ++ " { }"