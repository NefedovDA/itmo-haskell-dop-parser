{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpret
  ( Interpret(..)
  ) where

import Control.Monad (liftM2)
import Data.Map      (Map, insert, empty, fromList, (!?))
import Data.Monoid   (First(..))
import Data.Typeable (Typeable, (:~:)(..), eqT)
import GHC.Float     (int2Double)

import Kotlin.Dsl
import Kotlin.Utils

newtype Interpret a = Interpret { interpret :: a }

instance Kotlin Interpret where
  ktFile :: (Console c) => KtDeclarations Interpret c -> Interpret (KtFile c)
  ktFile declarations = Interpret $ do
    let scope = KtScope
          { sFun0 = foldr iterator readFuns  $ kdFun0 declarations
          , sFun1 = foldr iterator printFuns $ kdFun1 declarations
          , sFun2 = foldr iterator empty     $ kdFun2 declarations

          , sValue = empty
          , sVariable = empty
          }
    case interpret (ktCallFun0 "main") scope of
      HiddenIO KtUnitType ioMain -> ioMain
      _ -> error "Execution error: `main : () -> Unit` function not defined!"
    where
      iterator :: Interpret (KtFunData a) -> Map String a -> Map String a
      iterator iFun funs = uncurry insert (interpret iFun) funs

  ktFun0
    :: forall c. (Console c)
    => Name
    -> KtAnyType
    -> [Interpret (KtCommand c)]
    -> Interpret (KtFunData (KtFun0 c))
  ktFun0 name raType@(KtAnyType (rType :: KtType rT)) cmds =
    Interpret (fun2key name [], fun0)
    where
      fun0 :: (Console c) => (KtFun0 c)
      fun0 = mkFunBody rType cmds

  ktFun1
    :: forall c. (Console c)
    => Name
    -> KtFunArg
    -> KtAnyType
    -> [Interpret (KtCommand c)]
    -> Interpret (KtFunData (KtFun1 c))
  ktFun1 name (aName, aaType@(KtAnyType aType)) (KtAnyType rType) cmds =
    Interpret (fun2key name [aaType], fun1)
    where
      fun1 :: (KtFun1 c)
      fun1 = \scope a@(HiddenIO afType _) ->
        if afType @==@ aType
        then
          mkFunBody rType cmds $
            scope { sValue = insert aName a $ sValue scope }
        else
          error $
            "Incorrect type of argument "
              ++ "`" ++ aName ++ "` in a function "
              ++ "`" ++ name  ++ "`"

  ktFun2
    :: forall c. (Console c)
    => Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [Interpret (KtCommand c)]
    -> Interpret (KtFunData (KtFun2 c))
  ktFun2
    name
    (a1Name, a1aType@(KtAnyType a1Type))
    (a2Name, a2aType@(KtAnyType a2Type))
    (KtAnyType rType)
    cmds
      =
    Interpret (fun2key name [a1aType, a2aType], fun2)
    where
      fun2 :: (Console c) => (KtFun2 c)
      fun2 = \scope a1@(HiddenIO a1fType _) a2@(HiddenIO a2fType _) ->
        if a1fType @==@ a1Type && a2fType @==@ a2Type
        then
          mkFunBody rType cmds $ scope
            { sValue = insert a1Name a1 $ insert a2Name a2 $ sValue scope }
        else
          error $
            "Incorrect type of some argument in a function "
              ++ "`" ++ name  ++ "`"

  ktReturn :: Interpret (KtValue c) -> Interpret (KtCommand c)
  ktReturn = Interpret . KtCommandReturn . interpret
  
  ktValueCommand :: (Console c) => Interpret (KtValue c) -> Interpret (KtCommand c)
  ktValueCommand iv = Interpret . KtCommandStep $ \scope ->
    case interpret iv scope of
        HiddenIO _ ioa -> ioa >> return scope

  ktCallFun0 :: Name -> Interpret (KtValue c)
  ktCallFun0 name = Interpret $ \scope ->
    fromJust
      ("no function " ++ name ++ "()")
      (sFun0 scope !? fun2key name [])
      scope


  ktCallFun1 :: Name -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktCallFun1 name ia = Interpret $ \scope ->
    case interpret ia scope of
      ha@(HiddenIO aType _) ->
        fromJust
          ("no function " ++ name ++ "(" ++ show aType ++ ")")
          (sFun1 scope !? fun2key name [KtAnyType aType])
          scope ha

  ktCallFun2 :: Name -> Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktCallFun2 name ia1 ia2 = Interpret $ \scope ->
    case (interpret ia1 scope, interpret ia2 scope) of
      (ha1@(HiddenIO a1Type _), ha2@(HiddenIO a2Type _)) ->
        fromJust
          ("no function " ++ name ++ "(" ++ show a1Type ++ "," ++ show a2Type ++ ")")
          (sFun2 scope !? fun2key name [KtAnyType a1Type, KtAnyType a2Type])
          scope ha1 ha2

  ktAddition ::(Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktAddition = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (+)
    , bOnDouble = KtDoubleType `to` (+)
    , bOnString = KtStringType `to` (++)
    }

  ktDifferent :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktDifferent = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (-)
    , bOnDouble = KtDoubleType `to` (-)
    }

  ktMultiplication :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktMultiplication = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (*)
    , bOnDouble = KtDoubleType `to` (*)
    }

  ktRatio :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktRatio = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` div
    , bOnDouble = KtDoubleType `to` (/)
    }

  ktNegate :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNegate = interpretUnoOp $ unoOpPredator
    { uOnInt    = KtIntType    `to` negate
    , uOnDouble = KtDoubleType `to` negate
    }

  ktAnd :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktAnd = interpretBinOp $ binOpPredator { bOnBool = KtBoolType `to` (&&) }

  ktOr :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktOr = interpretBinOp $ binOpPredator { bOnBool = KtBoolType `to` (||) }

  ktNot :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNot = interpretUnoOp $ unoOpPredator { uOnBool = KtBoolType `to` not }

  ktEq :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktEq = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = KtBoolType `to` (==)
    , bOnDouble = KtBoolType `to` (==)
    , bOnBool   = KtBoolType `to` (==)
    , bOnString = KtBoolType `to` (==)
    }

  ktNotEq :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktNotEq = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = KtBoolType `to` (/=)
    , bOnDouble = KtBoolType `to` (/=)
    , bOnBool   = KtBoolType `to` (/=)
    , bOnString = KtBoolType `to` (/=)
    }

  ktGt :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktGt = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (>)
    , bOnDouble = KtBoolType `to` (>)
    , bOnString = KtBoolType `to` (>)
    , bOnBool   = KtBoolType `to` (>)
    }

  ktGte :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktGte = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (>=)
    , bOnDouble = KtBoolType `to` (>=)
    , bOnString = KtBoolType `to` (>=)
    , bOnBool   = KtBoolType `to` (>=)
    }

  ktLt :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktLt = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (<)
    , bOnDouble = KtBoolType `to` (<)
    , bOnString = KtBoolType `to` (<)
    , bOnBool   = KtBoolType `to` (<)
    }

  ktLte :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  ktLte = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (<=)
    , bOnDouble = KtBoolType `to` (<=)
    , bOnString = KtBoolType `to` (<=)
    , bOnBool   = KtBoolType `to` (<=)
    }

  ktInt :: (Console c) => Int -> Interpret (KtValue c)
  ktInt = interpretConstant KtIntType

  ktDouble :: (Console c) => Double -> Interpret (KtValue c)
  ktDouble = interpretConstant KtDoubleType

  ktString :: (Console c) => String -> Interpret (KtValue c)
  ktString = interpretConstant KtStringType

  ktBool :: (Console c) => Bool -> Interpret (KtValue c)
  ktBool = interpretConstant KtBoolType

  ktUnit :: (Console c) => () -> Interpret (KtValue c)
  ktUnit = interpretConstant KtUnitType

printFuns :: (Console c) => Map Name (KtFun1 c)
printFuns = fromList
  [ "print@i" `to` printFun1 consolePrint
  , "print@d" `to` printFun1 consolePrint
  , "print@s" `to` printFun1 consolePrint
  , "print@u" `to` printFun1 consolePrint
  , "print@b" `to` printFun1 consolePrint

  , "println@i" `to` printFun1 consolePrintln
  , "println@d" `to` printFun1 consolePrintln
  , "println@s" `to` printFun1 consolePrintln
  , "println@u" `to` printFun1 consolePrintln
  , "println@b" `to` printFun1 consolePrintln
  ]
  where
    printFun1
      :: (Console c) => (String -> c ()) -> KtScope c -> HiddenIO c -> HiddenIO c
    printFun1 sout = \_ (HiddenIO aType iov) ->
      HiddenIO KtUnitType $ iov >>= \v ->
        case aType of
          KtIntType    -> sout $ show v
          KtDoubleType -> sout $ show v
          KtStringType -> sout v
          KtUnitType   -> sout "kotlin.Unit"
          KtBoolType   -> sout $ if v then "true" else "false"

readFuns :: (Console c) => Map Name (KtFun0 c)
readFuns = fromList
  [ "readLine()!!.toInt@" `to` \_ ->
      HiddenIO KtIntType $ read @Int <$> consoleReadLine
  , "readLine()!!.toDouble@" `to` \_ ->
      HiddenIO KtDoubleType $ read @Double <$> consoleReadLine
  , "readLine@" `to` \_ ->
      HiddenIO KtStringType consoleReadLine
  ]

interpretConstant :: (Console c, Typeable a) => KtType a -> a -> Interpret (KtValue c)
interpretConstant aType a = Interpret $ \_ -> HiddenIO aType $ return a

mkFunBody :: (Console c, Typeable r) => KtType r -> [Interpret (KtCommand c)] -> KtScope c -> HiddenIO c
mkFunBody rType cmds funScope = HiddenIO rType $ do
  mbResult <- foldCommands funScope rType $ interpret <$> cmds
  case mbResult of
    Nothing -> case rType of
      KtUnitType -> return ()
      _          -> error "Missing return"
    Just r  -> return r

foldCommands :: (Console c, Typeable a) => (KtScope c) -> KtType a -> [KtCommand c] -> c (Maybe a)
foldCommands initScope (_ :: KtType a) cmds = foldCommands' initScope cmds Nothing
  where
    foldCommands' :: (Console c) => KtScope c -> [KtCommand c] -> Maybe a -> c (Maybe a)
    foldCommands' scope [] mbV = return mbV
    foldCommands' scope (cmd:cmds) Nothing =
      case cmd of
        KtCommandReturn underScope ->
          case underScope scope of
            HiddenIO _ (ioR :: c r) ->
              case eqT @a @r of
                Nothing   -> error "Incorrect type of return statement"
                Just Refl -> ioR >>= \r -> foldCommands' scope cmds $ Just r
        KtCommandStep ioS ->
          ioS scope >>= \newScope -> foldCommands' newScope cmds Nothing
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds Nothing >>= \mbR -> foldCommands' scope cmds mbR
    foldCommands' scope (cmd:cmds) jV@(Just v) =
      case cmd of
        KtCommandReturn underScope ->
          case underScope scope of
            HiddenIO _ (_ :: c r) ->
              case eqT @a @r of
                Nothing   -> error "Incorrect type of return statement"
                Just Refl -> foldCommands' scope cmds jV
        KtCommandStep ioS ->
          foldCommands' scope cmds jV
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds jV >> foldCommands' scope cmds jV

data UnoOpPredator i d b = UnoOpPredator
  { uOnInt    :: (KtType i, Int -> i)
  , uOnDouble :: (KtType d, Double -> d)
  , uOnBool   :: (KtType b, Bool -> b)
  }

unoOpPredator :: UnoOpPredator Int Double Bool
unoOpPredator = UnoOpPredator
  { uOnInt    = (KtIntType,    unoError)
  , uOnDouble = (KtDoubleType, unoError)
  , uOnBool   = (KtBoolType,   unoError)
  }
  where
    unoError :: a -> a
    unoError _ = error "Invalid type of operation argument"

interpretUnoOp
  :: (Console c, Typeable i, Typeable d, Typeable b)
  => UnoOpPredator i d b
  -> Interpret (KtValue c)
  -> Interpret (KtValue c)
interpretUnoOp predator iv =
  Interpret $ \scope ->
    case interpret iv scope of
      (HiddenIO KtIntType    iov) -> uOnInt    predator `attack` iov
      (HiddenIO KtDoubleType iov) -> uOnDouble predator `attack` iov
      (HiddenIO KtBoolType   iov) -> uOnBool   predator `attack` iov
      _                           -> error "Invalid type of operation argument"
  where
    attack :: (Console c, Typeable r) => (KtType r, a -> r) -> c a -> HiddenIO c
    attack (rType, f) iov = HiddenIO rType $ f <$> iov

data BinOpPredator i d b s u = BinOpPredator
  { bCanCast  :: Bool
  , bOnInt    :: (KtType i, Int -> Int -> i)
  , bOnDouble :: (KtType d, Double -> Double -> d)
  , bOnBool   :: (KtType b, Bool -> Bool -> b)
  , bOnString :: (KtType s, String -> String -> s)
  , bOnUnit   :: (KtType u, () -> () -> u)
  }

binOpPredator :: BinOpPredator Int Double Bool String ()
binOpPredator = BinOpPredator
  { bCanCast  = True
  , bOnInt    = (KtIntType,    binError)
  , bOnDouble = (KtDoubleType, binError)
  , bOnBool   = (KtBoolType,   binError)
  , bOnString = (KtStringType, binError)
  , bOnUnit   = (KtUnitType,   binError)
  }
  where
    binError :: a -> a -> a
    binError _ _ = error "Invalid types of operation arguments"

interpretBinOp
  :: (Console c, Typeable i, Typeable d, Typeable b, Typeable s, Typeable u)
  => BinOpPredator i d b s u
  -> Interpret (KtValue c)
  -> Interpret (KtValue c)
  -> Interpret (KtValue c)
interpretBinOp predator il ir = Interpret $ \scope ->
  case (interpret il scope, interpret ir scope) of
    (HiddenIO typeL iol, HiddenIO typeR ior) ->
      case (typeL, typeR) of
        (KtIntType,    KtIntType)    -> bOnInt    predator `attack` (iol, ior)
        (KtDoubleType, KtDoubleType) -> bOnDouble predator `attack` (iol, ior)
        (KtStringType, KtStringType) -> bOnString predator `attack` (iol, ior)
        (KtBoolType,   KtBoolType)   -> bOnBool   predator `attack` (iol, ior)
        (KtUnitType,   KtUnitType)   -> bOnUnit   predator `attack` (iol, ior)

        (KtDoubleType, KtIntType) ->
          if bCanCast predator
          then bOnDouble predator `attack` (iol, int2Double <$> ior)
          else binError
        (KtIntType, KtDoubleType) ->
          if bCanCast predator
          then bOnDouble predator `attack` (int2Double <$> iol, ior)
          else binError

        _ -> binError
  where
    attack :: (Console c, Typeable r) => (KtType r, a -> b -> r) -> (c a, c b) -> HiddenIO c
    attack (rType, f) (iol, ior) = HiddenIO rType $ liftM2 f iol ior

    binError :: a
    binError = error "Invalid types of operation arguments"

instance Show (KtType t) where
  show :: KtType t -> String
  show = \case
    KtIntType    -> "Int"
    KtDoubleType -> "Double"
    KtStringType -> "String"
    KtUnitType   -> "Unit"
    KtBoolType   -> "Bool"

infix 4 @==@
(@==@) :: (Typeable a, Typeable b) => KtType a -> KtType b -> Bool
(_ :: KtType a) @==@ (_ ::KtType b) =
  case eqT @a @b of
    Nothing -> False
    Just _  -> True

instance Show KtAnyType where
  show :: KtAnyType -> String
  show (KtAnyType aType) = show aType
