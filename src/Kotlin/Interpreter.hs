{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpreter
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
  ktFile :: KtDeclarations Interpret -> Interpret KtFile
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
      _ -> fail "Execution error: `main : () -> Unit` function not defined!"
    where
      iterator :: Interpret (KtFunData a) -> Map String a -> Map String a
      iterator iFun funs = uncurry insert (interpret iFun) funs

  ktFun0
    :: Name
    -> KtAnyType
    -> [Interpret KtCommand]
    -> Interpret (KtFunData KtFun0)
  ktFun0 name raType@(KtAnyType (rType :: KtType rT)) cmds =
    Interpret (fun2key name [], fun0)
    where
      fun0 :: KtFun0
      fun0 = mkFunBody rType cmds

  ktFun1
    :: Name
    -> KtFunArg
    -> KtAnyType
    -> [Interpret KtCommand]
    -> Interpret (KtFunData KtFun1)
  ktFun1 name (aName, aaType@(KtAnyType aType)) (KtAnyType rType) cmds =
    Interpret (fun2key name [aaType], fun1)
    where
      fun1 :: KtFun1
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
    :: Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [Interpret KtCommand]
    -> Interpret (KtFunData KtFun2)
  ktFun2
    name
    (a1Name, a1aType@(KtAnyType a1Type))
    (a2Name, a2aType@(KtAnyType a2Type))
    (KtAnyType rType)
    cmds
      =
    Interpret (fun2key name [a1aType, a2aType], fun2)
    where
      fun2 :: KtFun2
      fun2 = \scope a1@(HiddenIO a1fType _) a2@(HiddenIO a2fType _) ->
        if a1fType @==@ a1Type && a2fType @==@ a2Type
        then
          mkFunBody rType cmds $ scope
            { sValue = insert a1Name a1 $ insert a2Name a2 $ sValue scope }
        else
          error $
            "Incorrect type of some argument in a function "
              ++ "`" ++ name  ++ "`"

  ktReturn :: Interpret KtValue -> Interpret KtCommand
  ktReturn = Interpret . KtCommandReturn . interpret
  
  ktValueCommand :: Interpret KtValue -> Interpret KtCommand
  ktValueCommand iv = Interpret . KtCommandStep $ \scope ->
    case interpret iv scope of
        HiddenIO _ ioa -> ioa >> return scope

  ktCallFun0 :: Name -> Interpret KtValue
  ktCallFun0 name = Interpret $ \scope ->
    fromJust
      ("no function " ++ name ++ "()")
      (sFun0 scope !? fun2key name [])
      scope


  ktCallFun1 :: Name -> Interpret KtValue -> Interpret KtValue
  ktCallFun1 name ia = Interpret $ \scope ->
    case interpret ia scope of
      ha@(HiddenIO aType _) ->
        fromJust
          ("no function " ++ name ++ "(" ++ show aType ++ ")")
          (sFun1 scope !? fun2key name [KtAnyType aType])
          scope ha

  ktCallFun2 :: Name -> Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktCallFun2 name ia1 ia2 = Interpret $ \scope ->
    case (interpret ia1 scope, interpret ia2 scope) of
      (ha1@(HiddenIO a1Type _), ha2@(HiddenIO a2Type _)) ->
        fromJust
          ("no function " ++ name ++ "(" ++ show a1Type ++ "," ++ show a2Type ++ ")")
          (sFun2 scope !? fun2key name [KtAnyType a1Type, KtAnyType a2Type])
          scope ha1 ha2

  ktAddition :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktAddition = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (+)
    , bOnDouble = KtDoubleType `to` (+)
    , bOnString = KtStringType `to` (++)
    }

  ktDifferent :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktDifferent = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (-)
    , bOnDouble = KtDoubleType `to` (-)
    }

  ktMultiplication :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktMultiplication = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (*)
    , bOnDouble = KtDoubleType `to` (*)
    }

  ktRatio :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktRatio = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` div
    , bOnDouble = KtDoubleType `to` (/)
    }

  ktNegate :: Interpret KtValue -> Interpret KtValue
  ktNegate = interpretUnoOp $ unoOpPredator
    { uOnInt    = KtIntType    `to` negate
    , uOnDouble = KtDoubleType `to` negate
    }

  ktAnd :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktAnd = interpretBinOp $ binOpPredator { bOnBool = KtBoolType `to` (&&) }

  ktOr :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktOr = interpretBinOp $ binOpPredator { bOnBool = KtBoolType `to` (||) }

  ktNot :: Interpret KtValue -> Interpret KtValue
  ktNot = interpretUnoOp $ unoOpPredator { uOnBool = KtBoolType `to` not }

  ktEq :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktEq = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = KtBoolType `to` (==)
    , bOnDouble = KtBoolType `to` (==)
    , bOnBool   = KtBoolType `to` (==)
    , bOnString = KtBoolType `to` (==)
    }

  ktNotEq :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktNotEq = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = KtBoolType `to` (/=)
    , bOnDouble = KtBoolType `to` (/=)
    , bOnBool   = KtBoolType `to` (/=)
    , bOnString = KtBoolType `to` (/=)
    }

  ktGt :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktGt = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (>)
    , bOnDouble = KtBoolType `to` (>)
    , bOnString = KtBoolType `to` (>)
    , bOnBool   = KtBoolType `to` (>)
    }

  ktGte :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktGte = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (>=)
    , bOnDouble = KtBoolType `to` (>=)
    , bOnString = KtBoolType `to` (>=)
    , bOnBool   = KtBoolType `to` (>=)
    }

  ktLt :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktLt = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (<)
    , bOnDouble = KtBoolType `to` (<)
    , bOnString = KtBoolType `to` (<)
    , bOnBool   = KtBoolType `to` (<)
    }

  ktLte :: Interpret KtValue -> Interpret KtValue -> Interpret KtValue
  ktLte = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (<=)
    , bOnDouble = KtBoolType `to` (<=)
    , bOnString = KtBoolType `to` (<=)
    , bOnBool   = KtBoolType `to` (<=)
    }

  ktInt :: Int -> Interpret KtValue
  ktInt = interpretConstant KtIntType

  ktDouble :: Double -> Interpret KtValue
  ktDouble = interpretConstant KtDoubleType

  ktString :: String -> Interpret KtValue
  ktString = interpretConstant KtStringType

  ktBool :: Bool -> Interpret KtValue
  ktBool = interpretConstant KtBoolType

  ktUnit :: () -> Interpret KtValue
  ktUnit = interpretConstant KtUnitType

printFuns :: Map Name KtFun1
printFuns = fromList
  [ "print@i" `to` printFun1 putStr
  , "print@d" `to` printFun1 putStr
  , "print@s" `to` printFun1 putStr
  , "print@u" `to` printFun1 putStr
  , "print@b" `to` printFun1 putStr
  
  , "println@i" `to` printFun1 putStrLn
  , "println@d" `to` printFun1 putStrLn
  , "println@s" `to` printFun1 putStrLn
  , "println@u" `to` printFun1 putStrLn
  , "println@b" `to` printFun1 putStrLn
  ]
  where
    printFun1
      :: (String -> IO ()) -> KtScope -> HiddenIO -> HiddenIO
    printFun1 sout = \_ (HiddenIO aType iov) ->
      HiddenIO KtUnitType $ iov >>= \v ->
        case aType of
          KtIntType    -> sout $ show v
          KtDoubleType -> sout $ show v
          KtStringType -> sout v
          KtUnitType   -> sout "kotlin.Unit"
          KtBoolType   -> sout $ if v then "true" else "false"

readFuns :: Map Name KtFun0
readFuns = fromList
  [ "readLine()!!.toInt@" `to` \_ ->
      HiddenIO KtIntType $ read @Int <$> getLine
  , "readLine()!!.toDouble@" `to` \_ ->
      HiddenIO KtDoubleType $ read @Double <$> getLine
  , "readLine@" `to` \_ ->
      HiddenIO KtStringType getLine
  ]

interpretConstant :: (Typeable a) => KtType a -> a -> Interpret KtValue
interpretConstant aType a = Interpret $ \_ -> HiddenIO aType $ return a

mkFunBody :: (Typeable r) => KtType r -> [Interpret KtCommand] -> KtScope -> HiddenIO
mkFunBody rType cmds funScope = HiddenIO rType $ do
  mbResult <- foldCommands funScope rType $ interpret <$> cmds
  case mbResult of
    Nothing -> case rType of
      KtUnitType -> return ()
      _          -> fail "Missing return"
    Just r  -> return r

foldCommands :: Typeable a => KtScope -> KtType a -> [KtCommand] -> IO (Maybe a)
foldCommands initScope (_ :: KtType a) cmds = foldCommands' initScope cmds Nothing
  where
    foldCommands' :: KtScope -> [KtCommand] -> Maybe a -> IO (Maybe a)
    foldCommands' scope [] mbV = return mbV
    foldCommands' scope (cmd:cmds) Nothing =
      case cmd of
        KtCommandReturn underScope ->
          case underScope scope of
            HiddenIO _ (ioR :: IO r) ->
              case eqT @a @r of
                Nothing   -> fail "Incorrect type of return statement"
                Just Refl -> ioR >>= \r -> foldCommands' scope cmds $ Just r
        KtCommandStep ioS ->
          ioS scope >>= \newScope -> foldCommands' newScope cmds Nothing
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds Nothing >>= \mbR -> foldCommands' scope cmds mbR
    foldCommands' scope (cmd:cmds) jV@(Just v) =
      case cmd of
        KtCommandReturn underScope ->
          case underScope scope of
            HiddenIO _ (_ :: IO r) ->
              case eqT @a @r of
                Nothing   -> fail "Incorrect type of return statement"
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
  :: (Typeable i, Typeable d, Typeable b)
  => UnoOpPredator i d b
  -> Interpret KtValue
  -> Interpret KtValue
interpretUnoOp predator iv =
  Interpret $ \scope ->
    case interpret iv scope of
      (HiddenIO KtIntType    iov) -> uOnInt    predator `attack` iov
      (HiddenIO KtDoubleType iov) -> uOnDouble predator `attack` iov
      (HiddenIO KtBoolType   iov) -> uOnBool   predator `attack` iov
      _                           -> error "Invalid type of operation argument"
  where
    attack :: (Typeable r) => (KtType r, a -> r) -> IO a -> HiddenIO
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
  :: (Typeable i, Typeable d, Typeable b, Typeable s, Typeable u)
  => BinOpPredator i d b s u
  -> Interpret KtValue
  -> Interpret KtValue
  -> Interpret KtValue
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
    attack :: (Typeable r) => (KtType r, a -> b -> r) -> (IO a, IO b) -> HiddenIO
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
