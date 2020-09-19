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
    intercalate "\n" $ flipPrint offset <$> fnDecl

  ktFun
    :: Name
    -> [KtFunArg]
    -> KtAnyType
    -> [Printer (KtCommand c)]
    -> Printer (KtFunData c)
  ktFun name args rType cmds =
    printKtFun name args rType cmds

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

  ktCallFun :: Name -> [Printer (KtValue c)] -> Printer (KtValue c)
  ktCallFun name args = printCallFun name args

  ktFor
    :: Name
    -> Printer (KtValue c)
    -> Printer (KtValue c)
    -> [Printer (KtCommand c)]
    -> Printer (KtCommand c)
  ktFor iName from to cmds = Printer $ \offset ->
    getOffset offset ++ "for (" ++ iName ++ " in " ++ show from ++ ".." ++ show to ++ ") {\n"
      ++ printCommands (succ offset) cmds
      ++ getOffset offset ++ "}"

  ktIf
    :: [(Printer (KtValue c), [Printer (KtCommand c)])]
    -> [Printer (KtCommand c)]
    -> Printer (KtCommand c)
  ktIf branches elseBranch = Printer $ \offset ->
    getOffset offset ++ concatMap (printBranch offset) branches ++
    "{\n" ++ printCommands (succ offset) elseBranch ++ getOffset offset ++ "}"
    where
      printBranch :: Int -> (Printer (KtValue c), [Printer (KtCommand c)]) -> String
      printBranch offset (condition, cmds) =
        "if (" ++ show condition ++ ") {\n"
          ++ printCommands (succ offset) cmds
          ++ getOffset offset ++ "}\n"
          ++ getOffset offset ++ "else "
  
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

printCommands :: Int -> [Printer (KtCommand c)] -> String
printCommands offset cmds =
  foldMap (\p -> flipPrint offset p ++ "\n") cmds

printKtFun
  :: Name
  -> [KtFunArg]
  -> KtAnyType
  -> [Printer (KtCommand c)]
  -> Printer (KtFunData fun)
printKtFun name args rType cmds = Printer $ \offset ->
  getOffset offset ++ "fun " ++ name ++ "(" ++ (intercalate ", " $ showArg <$> args) ++ "): "
    ++ show rType ++ " {\n"
    ++ printCommands (succ offset) cmds
    ++ getOffset offset ++ "}\n"
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
