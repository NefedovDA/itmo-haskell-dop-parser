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
  
  ktReturn :: Printer KtValue -> Printer KtCommand
  ktReturn result = (Printer "return ") <> castP result <> (Printer ";")

  ktValueCommand :: Printer KtValue -> Printer KtCommand
  ktValueCommand v = castP v <> (Printer ";")

  ktCallFun0 :: Name -> Printer KtValue
  ktCallFun0 name = printCallFun name []

  ktCallFun1 :: Name -> Printer KtValue -> Printer KtValue
  ktCallFun1 name arg = printCallFun name [arg]

  ktCallFun2 :: Name -> Printer KtValue -> Printer KtValue -> Printer KtValue
  ktCallFun2 name arg1 arg2 = printCallFun name [arg1, arg2]

  ktAddition :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktAddition = printBinOp "+"

  ktDifferent :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktDifferent = printBinOp "-"

  ktMultiplication :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktMultiplication = printBinOp "*"

  ktRatio :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktRatio = printBinOp "/"

  ktNegate :: Printer KtValue -> Printer KtValue
  ktNegate = printUnoOp "-"

  ktAnd :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktAnd = printBinOp "&&"

  ktOr :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktOr = printBinOp "||"

  ktNot :: Printer KtValue -> Printer KtValue
  ktNot = printUnoOp "!"

  ktEq :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktEq = printBinOp "=="

  ktNotEq :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktNotEq = printBinOp "!="

  ktGt :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktGt = printBinOp ">"

  ktGte :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktGte = printBinOp ">="

  ktLt :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktLt = printBinOp "<"

  ktLte :: Printer KtValue -> Printer KtValue -> Printer KtValue
  ktLte = printBinOp "<="

  ktInt :: Int -> Printer KtValue
  ktInt = Printer . show
  
  ktDouble :: Double -> Printer KtValue
  ktDouble = Printer . show
  
  ktString :: String -> Printer KtValue
  ktString s = Printer $ "\"" ++ s ++  "\""
  
  ktBool :: Bool -> Printer KtValue
  ktBool b = Printer $ if b then "true" else "false"
  
  ktUnit :: () -> Printer KtValue
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

printKtFun
  :: Name
  -> [KtFunArg]
  -> KtAnyType
  -> [Printer KtCommand]
  -> Printer (KtFunData fun)
printKtFun name args rType cmds = Printer $ 
  "fun " ++ name ++ "(" ++ (intercalate ", " $ showKtArg <$> args) ++ "): "
    ++ show rType
    ++ " {\n"
    ++ foldMap (\p -> runPrint p ++ "\n") cmds
    ++ "}\n"

printBinOp :: Name -> Printer a -> Printer b -> Printer c
printBinOp opName ivl ivr =
  (Printer "(")
    <> castP ivl
    <> (Printer $ " " ++ opName ++ " ")
    <> castP ivr
    <> (Printer ")")

printUnoOp :: Name -> Printer a -> Printer b
printUnoOp opName iv = (Printer opName) <> castP iv

printCallFun :: Name -> [Printer a] -> Printer b
printCallFun name args = Printer $
  name ++ "(" ++ (intercalate ", " $ runPrint <$> args) ++ ")"
