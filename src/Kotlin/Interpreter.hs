{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpreter
  ( Interpret(..)
  ) where

import Data.Map      (Map, insert, empty, (!?))
import Data.Typeable (Typeable, (:~:)(..), eqT)
import GHC.Float     (int2Double)

import Kotlin.Dsl
import Kotlin.Utils

newtype Interpret a = Interpret { interpret :: a }

instance Kotlin Interpret where
  ktFile :: KtDeclarations Interpret -> Interpret KtFile
  ktFile declarations = Interpret $ do
    let scope = KtScope
          { sFun0 = foldr iterator empty $ kdFun0 declarations
          , sFun1 = foldr iterator empty $ kdFun1 declarations
          , sFun2 = foldr iterator empty $ kdFun2 declarations
          
          , sValue = empty
          , sVariable = empty
          }
    case getFun0 scope "main" KtUnitType of
      Nothing -> fail "Execution error: `main : () -> Unit` function not defined!"
      Just f -> f scope
    where
      iterator :: Interpret (KtFunData a) -> Map String a -> Map String a
      iterator iFun funs =
        let (name, types, fun) = interpret iFun
          in insert (fun2key name types) fun funs

  ktFun0
    :: Name
    -> KtAnyType
    -> [Interpret KtCommand]
    -> Interpret (KtFunData KtFun0)
  ktFun0 name raType@(KtAnyType (rType :: KtType rT)) cmds =
    Interpret (name, [raType], fun0)
    where
      fun0 :: KtFun0
      fun0 = KtFun0 $ mkFunBody rType cmds

  ktFun1
    :: Name
    -> KtFunArg
    -> KtAnyType
    -> [Interpret KtCommand]
    -> Interpret (KtFunData KtFun1)
  ktFun1
    name
    (aName, aaType@(KtAnyType (aType :: KtType aT)))
    raType@(KtAnyType (rType :: KtType rT))
    cmds
      =
    Interpret (name, [aaType, raType], fun1)
    where
      fun1 :: KtFun1
      fun1 = KtFun1 $ \scope a ->
        return scope { sValue = insert aName (KtVariable aType a) $ sValue scope }
          >>= mkFunBody rType cmds

  ktFun2
    :: Name
    -> KtFunArg
    -> KtFunArg
    -> KtAnyType
    -> [Interpret KtCommand]
    -> Interpret (KtFunData KtFun2)
  ktFun2
    name
    (a1Name, a1aType@(KtAnyType (a1Type :: KtType a1T)))
    (a2Name, a2aType@(KtAnyType (a2Type :: KtType a2T)))
    raType@(KtAnyType (rType :: KtType rT))
    cmds
      =
    Interpret (name, [a1aType, a2aType, raType], fun2)
    where
      fun2 :: KtFun2
      fun2 = KtFun2 $ \scope a1 a2 ->
        return scope
          { sValue = insert a1Name (KtVariable a1Type a1) $
                     insert a2Name (KtVariable a2Type a2) $ sValue scope
          }
          >>= mkFunBody rType cmds

  ktReturn :: Interpret KtAnyValue -> Interpret KtCommand
  ktReturn = Interpret . KtCommandReturn . interpret

  ktAddition :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktAddition = interpretBinOp $ binOpPredator
    { bOnInt    = (+)
    , bOnDouble = (+)
    , bOnString = (++)
    }

  ktDifferent :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktDifferent = interpretBinOp $ binOpPredator
    { bOnInt    = (-)
    , bOnDouble = (-)
    }

  ktMultiplication :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktMultiplication = interpretBinOp $ binOpPredator
    { bOnInt    = (*)
    , bOnDouble = (*)
    }

  ktRatio :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktRatio = interpretBinOp $ binOpPredator
    { bOnInt    = div
    , bOnDouble = (/)
    }

  ktNegate :: Interpret KtAnyValue -> Interpret KtAnyValue
  ktNegate = interpretUnoOp $ unoOpPredator
    { uOnInt    = negate
    , uOnDouble = negate
    }

  ktAnd :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktAnd = interpretBinOp $ binOpPredator { bOnBool = (&&) }

  ktOr :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktOr = interpretBinOp $ binOpPredator { bOnBool = (||) }
  
  ktNot :: Interpret KtAnyValue -> Interpret KtAnyValue
  ktNot = interpretUnoOp $ unoOpPredator { uOnBool = not }

  ktEq :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktEq = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = (==)
    , bOnDouble = (==)
    , bOnBool   = (==)
    , bOnString = (==)
    }

  ktNotEq :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktNotEq = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = (/=)
    , bOnDouble = (/=)
    , bOnBool   = (/=)
    , bOnString = (/=)
    }

  ktGt :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktGt = interpretBinOp $ binOpPredator
    { bOnInt    = (>)
    , bOnDouble = (>)
    , bOnString = (>)
    , bOnBool   = (>)
    }

  ktGte :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktGte = interpretBinOp $ binOpPredator
    { bOnInt    = (>=)
    , bOnDouble = (>=)
    , bOnString = (>=)
    , bOnBool   = (>=)
    }

  ktLt :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktLt = interpretBinOp $ binOpPredator
    { bOnInt    = (<)
    , bOnDouble = (<)
    , bOnString = (<)
    , bOnBool   = (<)
    }

  ktLte :: Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktLte = interpretBinOp $ binOpPredator
    { bOnInt    = (<=)
    , bOnDouble = (<=)
    , bOnString = (<=)
    , bOnBool   = (<=)
    }

  ktInt :: Int -> Interpret KtAnyValue
  ktInt = interpretConstant

  ktDouble :: Double -> Interpret KtAnyValue
  ktDouble = interpretConstant

  ktString :: String -> Interpret KtAnyValue
  ktString = interpretConstant

  ktBool :: Bool -> Interpret KtAnyValue
  ktBool = interpretConstant

  ktUnit :: () -> Interpret KtAnyValue
  ktUnit = interpretConstant

interpretConstant :: (Typeable a) => a -> Interpret KtAnyValue
interpretConstant a = Interpret . KtAnyValue $ \_ -> return a

mkFunBody :: (Typeable r) => KtType r -> [Interpret KtCommand] -> KtScope -> IO r
mkFunBody (rType :: KtType rT) cmds funScope = do
  mbResult <- foldCommands funScope rType $ interpret <$> cmds
  case mbResult of
    Nothing -> case eqT @rT @() of
      Nothing   -> fail "Missing return"
      Just Refl -> return ()
    Just r  -> return r

getFun0 :: Typeable r => KtScope -> Name -> KtType r -> Maybe (KtScope -> IO r)
getFun0 KtScope { sFun0 = funs } name (rType :: KtType rT) = do
  KtFun0 (fun0 :: KtScope -> IO r) <- funs !? (fun2key name [KtAnyType rType])
  Refl <- eqT @r @rT
  return fun0

getFun1
  :: (Typeable a, Typeable r)
  => KtScope
  -> Name
  -> KtType a
  -> KtType r
  -> Maybe (KtScope -> a -> IO r)
getFun1
  KtScope { sFun1 = funs }
  name
  (aType :: KtType aT)
  (rType :: KtType rT)
    = do
  KtFun1 (fun1 :: KtScope -> a -> IO r) <-
    funs !? (fun2key name [KtAnyType aType, KtAnyType rType])
  Refl <- eqT @a @aT
  Refl <- eqT @r @rT
  return undefined

getFun2
  :: (Typeable a1, Typeable a2, Typeable r)
  => KtScope
  -> Name
  -> KtType a1
  -> KtType a2
  -> KtType r
  -> Maybe (KtScope -> a1 -> a2 -> IO r)
getFun2 
  KtScope { sFun2 = funs }
  name
  (a1Type :: KtType a1T)
  (a2Type :: KtType a2T)
  (rType  :: KtType rT)
    = do
  KtFun2 (fun2 :: KtScope -> a1 -> a2 -> IO r) <-
    funs !? (fun2key name [KtAnyType a1Type, KtAnyType a2Type, KtAnyType rType])
  Refl <- eqT @a1 @a1T
  Refl <- eqT @a2 @a2T
  Refl <- eqT @r  @rT
  return fun2

foldCommands :: Typeable a => KtScope -> KtType a -> [KtCommand] -> IO (Maybe a)
foldCommands initScope (ktType :: KtType a) cmds = foldCommands' initScope cmds Nothing
  where
    foldCommands' :: KtScope -> [KtCommand] -> Maybe a -> IO (Maybe a)
    foldCommands' scope [] mbV = return mbV
    foldCommands' scope (cmd:cmds) Nothing =
      case cmd of
        KtCommandReturn (KtAnyValue (ioR :: KtScope -> IO r)) ->
          case eqT @a @r of
            Nothing   -> fail "Incorrect type of return statement"
            Just Refl -> ioR scope >>= \r -> foldCommands' scope cmds $ Just r
        KtCommandStep ioS ->
          ioS scope >>= \(newScope, _) -> foldCommands' newScope cmds Nothing
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds Nothing >>= \mbR -> foldCommands' scope cmds mbR
    foldCommands' scope (cmd:cmds) jV@(Just v) =
      case cmd of
        KtCommandReturn (KtAnyValue (ioR :: KtScope -> IO r)) ->
          case eqT @a @r of
            Nothing   -> fail "Incorrect type of return statement"
            Just Refl -> foldCommands' scope cmds jV
        KtCommandStep ioS ->
          foldCommands' scope cmds jV
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds jV >> foldCommands' scope cmds jV

castAnyType :: (Typeable a) => KtType a -> KtAnyValue -> Maybe (KtValue a)
castAnyType (_ :: KtType aT) (KtAnyValue (r :: KtValue anyT)) = do
  Refl <- eqT @aT @anyT
  pure r

data UnoOpPredator i d b = UnoOpPredator
  { uOnInt    :: Int -> i
  , uOnDouble :: Double -> d
  , uOnBool   :: Bool -> b
  }

unoOpPredator :: UnoOpPredator Int Double Bool
unoOpPredator = UnoOpPredator
  { uOnInt    = unoError
  , uOnDouble = unoError
  , uOnBool   = unoError
  }
  where
    unoError :: a -> a
    unoError _ = error "Invalid type of operation argument"

interpretUnoOp
  :: (Typeable i, Typeable d, Typeable b)
  => UnoOpPredator i d b
  -> Interpret KtAnyValue
  -> Interpret KtAnyValue
interpretUnoOp predator iv = let av = interpret iv in
  case (castAnyType KtIntType av, castAnyType KtDoubleType av, castAnyType KtBoolType av) of
    (Just iov, _, _) -> ans iov $ uOnInt    predator
    (_, Just iov, _) -> ans iov $ uOnDouble predator
    (_, _, Just iov) -> ans iov $ uOnBool   predator
    _                -> error "Invalid type of argument"
    where
      ans :: (Typeable a, Typeable b) => KtValue a -> (a -> b) -> Interpret KtAnyValue
      ans iov op = Interpret . KtAnyValue $ \scope -> do
        v <- iov scope
        return $ op v

data BinOpPredator i d b s u = BinOpPredator
  { bCanCast  :: Bool
  , bOnInt    :: (Int -> Int -> i)
  , bOnDouble :: (Double -> Double -> d)
  , bOnBool   :: (Bool -> Bool -> b)
  , bOnString :: (String -> String -> s)
  , bOnUnit   :: (() -> () -> u)
  }

binOpPredator :: BinOpPredator Int Double Bool String ()
binOpPredator = BinOpPredator
  { bCanCast  = True
  , bOnInt    = binError
  , bOnDouble = binError
  , bOnBool   = binError
  , bOnString = binError
  , bOnUnit   = binError
  }
  where
    binError :: a -> a -> a
    binError _ _ = error "Invalid types of operation arguments"

interpretBinOp
  :: (Typeable i, Typeable d, Typeable b, Typeable s, Typeable u)
  => BinOpPredator i d b s u
  -> Interpret KtAnyValue
  -> Interpret KtAnyValue
  -> Interpret KtAnyValue
interpretBinOp predator il ir =
  let al = interpret il
      ar = interpret ir
      typeCasts =
        ( castAnyType KtIntType    al
        , castAnyType KtDoubleType al
        , castAnyType KtBoolType   al
        , castAnyType KtStringType al
        , castAnyType KtUnitType   al
        , castAnyType KtIntType    ar
        , castAnyType KtDoubleType ar
        , castAnyType KtBoolType   ar
        , castAnyType KtStringType ar
        , castAnyType KtUnitType   ar
        )
   in case typeCasts of
        (Just iol, _, _, _, _, Just ior, _, _, _, _) -> ans iol ior $ bOnInt    predator
        (_, Just iol, _, _, _, _, Just ior, _, _, _) -> ans iol ior $ bOnDouble predator
        (_, _, Just iol, _, _, _, _, Just ior, _, _) -> ans iol ior $ bOnBool   predator
        (_, _, _, Just iol, _, _, _, _, Just ior, _) -> ans iol ior $ bOnString predator
        (_, _, _, _, Just iol, _, _, _, _, Just ior) -> ans iol ior $ bOnUnit   predator
        (Just iol, _, _, _, _, _, Just ior, _, _, _) ->
          if bCanCast predator
          then ans (castIO iol) ior $ bOnDouble predator
          else binError
        (_, Just iol, _, _, _, Just ior, _, _, _, _) ->
          if bCanCast predator
          then ans iol (castIO ior) $ bOnDouble predator
          else binError
        _ -> binError
    where
      binError :: a
      binError = error "Invalid types of operation arguments"
      
      castIO :: KtValue Int -> KtValue Double
      castIO io = \scope -> io scope >>= return . int2Double

      ans 
        :: (Typeable a, Typeable b, Typeable c) 
        => KtValue a -> KtValue b -> (a -> b -> c) -> Interpret KtAnyValue
      ans iol ior op = Interpret . KtAnyValue $ \scope -> do
        vl <- iol scope
        vr <- ior scope
        return $ op vl vr
        
