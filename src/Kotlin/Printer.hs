{-# LANGUAGE GADTs                      #-}
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
  ktFile :: KtDeclarations Printer -> Printer KtFile
  ktFile fnDecl = Printer $
    (intercalate "\n" $ runPrint <$> kdFun0 fnDecl) ++
    (intercalate "\n" $ runPrint <$> kdFun1 fnDecl) ++
    (intercalate "\n" $ runPrint <$> kdFun2 fnDecl)

  ktFun0
    :: Name
    -> KtAnyType
    -> [Printer KtCommand]
    -> Printer (KtFunData KtFun0)
  ktFun0 name rType cmds =
    printKtFun name [] rType cmds

  ktFun1
    :: Name
    -> KtFunArg
    -> KtAnyType
    -> [Printer KtCommand]
    -> Printer (KtFunData KtFun1)
  ktFun1 name arg rType cmds =
    printKtFun name [arg] rType cmds

  ktFun2
    :: Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [Printer KtCommand]
    -> Printer (KtFunData KtFun2)
  ktFun2 name arg1 arg2 rType cmds =
    printKtFun name [arg1, arg2] rType cmds
  
  ktReturn :: Printer KtAnyValue -> Printer KtCommand
  ktReturn result = (Printer "return ") <> castP result <> (Printer ";")

  ktInt :: Int -> Printer (KtValue Int)
  ktInt = Printer . show
  
  ktDouble :: Double -> Printer (KtValue Double)
  ktDouble = Printer . show
  
  ktString :: String -> Printer (KtValue String)
  ktString s = Printer $ "\"" ++ s ++  "\""
  
  ktBool :: Bool -> Printer (KtValue Bool)
  ktBool b = Printer $ if b then "true" else "false"
  
  ktUnit :: () -> Printer (KtValue ())
  ktUnit () = Printer $ "Unit"

instance Show (KtType t) where
  show :: KtType t -> String
  show = \case
    KtIntType    -> "Int"
    KtDoubleType -> "Double"
    KtStringType -> "String"
    KtUnitType   -> "Unit"
    KtBoolType   -> "Bool"

instance Show KtAnyType where
  show :: KtAnyType -> String
  show (KtAnyType ktType) = show ktType

showKtArg :: KtFunArg -> String
showKtArg (name, aType) = name ++ ": " ++ show aType

printKtFun :: Name -> [KtFunArg] -> KtAnyType -> [Printer KtCommand] -> Printer (KtFunData fun)
printKtFun name args rType cmds = Printer $ 
  "fun " ++ name ++ "(" ++ (intercalate ", " $ showKtArg <$> args) ++ "): " ++ show rType
    ++ " {\n"
    ++ foldMap (\p -> runPrint p ++ "\n") cmds
    ++ "}\n"
