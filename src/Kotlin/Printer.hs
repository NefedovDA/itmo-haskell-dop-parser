{-# LANGUAGE GADTs        #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Kotlin.Printer
  ( Printer (..)
  ) where

import Data.List (intercalate)

import Kotlin.Dsl

-- | Type for format-print some code.
newtype Printer a = Printer
  { runPrint
      :: Int     -- ^ Offset level.
      -> String  -- ^ String representation of @a@.
  }

-- | @show@ === @runPrint@ with zero offset.
instance Show (Printer a) where
  show :: Printer a -> String
  show = flipPrint 0

-- | Specification how Kotlin should be format-printed.
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
  ktFun name args rType cmds = Printer $ \offset ->
    getOffset offset ++ "fun " ++ name
      ++ "(" ++ (intercalate ", " $ showArg <$> args) ++ "): "
      ++ show rType ++ " {\n"
      ++ printCommands (succ offset) cmds
      ++ getOffset offset ++ "}\n"
    where
      showArg :: KtFunArg -> String
      showArg (name, aType) = name ++ ": " ++ show aType

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
  ktCallFun name args = Printer $ \offset ->
    getOffset offset ++ name ++ "(" ++ (intercalate ", " $ show <$> args) ++ ")"

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

  ktNegate :: Printer (KtValue c) -> Printer (KtValue c)
  ktNegate = printUnoOp "-"

  ktNot :: Printer (KtValue c) -> Printer (KtValue c)
  ktNot = printUnoOp "!"

  (@*@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@*@) = printBinOp "*"

  (@/@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@/@) = printBinOp "/"

  (@+@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@+@) = printBinOp "+"

  (@-@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@-@) = printBinOp "-"

  (@>@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@>@) = printBinOp ">"

  (@>=@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@>=@) = printBinOp ">="

  (@<@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@<@) = printBinOp "<"

  (@<=@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@<=@) = printBinOp "<="

  (@==@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@==@) = printBinOp "=="

  (@!=@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@!=@) = printBinOp "!="

  (@&&@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@&&@) = printBinOp "&&"

  (@||@) :: Printer (KtValue c) -> Printer (KtValue c) -> Printer (KtValue c)
  (@||@) = printBinOp "||"

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

-- | Binding for @flip runPrint@.
flipPrint :: Int -> Printer a -> String
flipPrint = flip runPrint

-- | Return string offset of given level.
getOffset :: Int -> String
getOffset offset = take (offset * 2) $ repeat ' '

-- | Print list of commands with given offset level.
printCommands :: Int -> [Printer (KtCommand c)] -> String
printCommands offset cmds =
  foldMap (\p -> flipPrint offset p ++ "\n") cmds

-- | Specify printing unary operations.
printUnoOp :: Name -> Printer a -> Printer b
printUnoOp opName iv = Printer $ \offset ->
  getOffset offset ++ opName ++ show iv

-- | Specify printing binary operations.
printBinOp :: Name -> Printer a -> Printer b -> Printer c
printBinOp opName ivl ivr = Printer $ \offset ->
  getOffset offset ++ "(" ++ show ivl ++ " " ++ opName ++ " " ++ show ivr ++ ")"
