{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}

module Kotlin.Printer
  ( Printer (..)
  ) where

import Data.List (intercalate)

import Kotlin.Dsl

newtype Printer a = Printer { runPrint :: Int -> String }

instance Show (Printer a) where
  show :: Printer a -> String
  show = flipPrint 0

instance Kotlin Printer where
  ktFile :: KtDeclarations Printer c -> Printer (KtFile c)
  ktFile fnDecl = Printer $ \offset ->
    intercalate "\n" $ concat
      [ flipPrint offset <$> kdFun0 fnDecl
      , flipPrint offset <$> kdFun1 fnDecl
      , flipPrint offset <$> kdFun2 fnDecl
      ]

  ktFun0
    :: Name
    -> KtAnyType
    -> [Printer (KtCommand c)]
    -> Printer (KtFunData (KtFun0 c))
  ktFun0 name rType cmds =
    printKtFun name [] rType cmds

  ktFun1
    :: Name
    -> KtFunArg
    -> KtAnyType
    -> [Printer (KtCommand c)]
    -> Printer (KtFunData (KtFun1 c))
  ktFun1 name arg rType cmds =
    printKtFun name [arg] rType cmds

  ktFun2
    :: Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [Printer (KtCommand c)]
    -> Printer (KtFunData (KtFun2 c))
  ktFun2 name arg1 arg2 rType cmds =
    printKtFun name [arg1, arg2] rType cmds

  ktInitVariable
    :: Bool
    -> Name
    -> KtAnyType
    -> Printer (KtValue c)
    -> Printer (KtCommand c)
  ktInitVariable isConstant name vType value = Printer $ \offset ->
    getOffset offset ++ (if isConstant then "val" else "var") ++ " "
      ++ name ++ ": " ++ show vType ++ " = "
      ++ show value ++ ";"
  
  ktSetVariable :: (Console c) => Name -> Printer (KtValue c) -> Printer (KtCommand c)
  ktSetVariable name value = Printer $ \offset ->
    getOffset offset ++ name ++ " = " ++ show value ++ ";"
  
  ktReturn :: Printer (KtValue c) -> Printer (KtCommand c)
  ktReturn result = Printer $ \offset ->
    getOffset offset ++ "return " ++ show result ++ ";"

  ktValueCommand :: Printer (KtValue c) -> Printer (KtCommand c)
  ktValueCommand v = Printer $ \offset ->
    flipPrint offset v ++ ";"

  ktCallFun0 :: Name -> Printer (KtValue c)
  ktCallFun0 name = printCallFun name []

  ktCallFun1 :: Name -> Printer (KtValue c) -> Printer (KtValue c)
  ktCallFun1 name arg = printCallFun name [arg]

  ktCallFun2 :: Name -> Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktCallFun2 name arg1 arg2 = printCallFun name [arg1, arg2]
  
  ktReadVariable :: (Console c) => Name -> Printer (KtValue c)
  ktReadVariable name = Printer $ \offset ->
    getOffset offset ++ name

  ktAddition :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktAddition = printBinOp "+"

  ktDifferent :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktDifferent = printBinOp "-"

  ktMultiplication :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktMultiplication = printBinOp "*"

  ktRatio :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktRatio = printBinOp "/"

  ktNegate :: Printer (KtValue c) -> Printer (KtValue c)
  ktNegate = printUnoOp "-"

  ktAnd :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktAnd = printBinOp "&&"

  ktOr :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktOr = printBinOp "||"

  ktNot :: Printer (KtValue c) -> Printer (KtValue c)
  ktNot = printUnoOp "!"

  ktEq :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktEq = printBinOp "=="

  ktNotEq :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktNotEq = printBinOp "!="

  ktGt :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktGt = printBinOp ">"

  ktGte :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktGte = printBinOp ">="

  ktLt :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktLt = printBinOp "<"

  ktLte :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  ktLte = printBinOp "<="

  ktInt :: Int -> Printer (KtValue c)
  ktInt i = Printer $ \offset ->
    getOffset offset ++ show i
  
  ktDouble :: Double -> Printer (KtValue c)
  ktDouble d = Printer $ \offset ->
    getOffset offset ++ show d
  
  ktString :: String -> Printer (KtValue c)
  ktString s = Printer $ \offset ->
    getOffset offset ++ "\"" ++ s ++  "\""
  
  ktBool :: Bool -> Printer (KtValue c)
  ktBool b = Printer $ \offset ->
    getOffset offset ++ if b then "true" else "false"
  
  ktUnit :: () -> Printer (KtValue c)
  ktUnit () = Printer $ \offset ->
    getOffset offset ++ "Unit"

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

flipPrint :: Int -> Printer a -> String
flipPrint = flip runPrint

getOffset :: Int -> String
getOffset offset = take (offset * 2) $ repeat ' '

printKtFun
  :: Name
  -> [KtFunArg]
  -> KtAnyType
  -> [Printer (KtCommand c)]
  -> Printer (KtFunData fun)
printKtFun name args rType cmds = Printer $ \offset ->
  getOffset offset ++ "fun " ++ name ++ "(" ++ (intercalate ", " $ showArg <$> args) ++ "): "
    ++ show rType ++ " {\n"
    ++ foldMap (\p -> flipPrint (succ offset) p ++ "\n") cmds
    ++ "}\n"
  where
    showArg :: KtFunArg -> String
    showArg (name, aType) = name ++ ": " ++ show aType

printUnoOp :: Name -> Printer a -> Printer b
printUnoOp opName iv = Printer $ \offset ->
  getOffset offset ++ opName ++ show iv

printBinOp :: Name -> Printer a -> Printer b -> Printer c
printBinOp opName ivl ivr = Printer $ \offset ->
  getOffset offset ++ "(" ++ show ivl ++ " " ++ opName ++ " " ++ show ivr ++ ")"

printCallFun :: Name -> [Printer a] -> Printer b
printCallFun name args = Printer $ \offset ->
  getOffset offset ++ name ++ "(" ++ (intercalate ", " $ show <$> args) ++ ")"
