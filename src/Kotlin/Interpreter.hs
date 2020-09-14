{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpreter
  ( Interpret(..)
  ) where

import Data.Map      (Map, insert, empty, fromList, (!?))
import Data.Typeable (Typeable, (:~:)(..), eqT)
import GHC.Float     (int2Double)

import Kotlin.Dsl
import Kotlin.Utils
import Data.Monoid (First(..))
import Control.Monad (liftM2)

newtype Interpret a = Interpret { interpret :: a }

printFuns :: Map Name KtFun1
printFuns = fromList
  [ "print@i" `to` KtFun1 (printFun1 putStr KtIntType)
  , "print@d" `to` KtFun1 (printFun1 putStr KtDoubleType)
  , "print@s" `to` KtFun1 (printFun1 putStr KtStringType)
  , "print@u" `to` KtFun1 (printFun1 putStr KtUnitType)
  , "print@b" `to` KtFun1 (printFun1 putStr KtBoolType)
  
  , "println@i" `to` KtFun1 (printFun1 putStrLn KtIntType)
  , "println@d" `to` KtFun1 (printFun1 putStrLn KtDoubleType)
  , "println@s" `to` KtFun1 (printFun1 putStrLn KtStringType)
  , "println@u" `to` KtFun1 (printFun1 putStrLn KtUnitType)
  , "println@b" `to` KtFun1 (printFun1 putStrLn KtBoolType)
  ]
  where
    printFun1
      :: (Show a, Typeable a) => (String -> IO ()) -> KtType a -> KtScope -> a -> IO ()
    printFun1 sout = \case
      KtIntType    -> \_ i -> sout $ show i
      KtDoubleType -> \_ d -> sout $ show d
      KtStringType -> \_ s -> sout s
      KtUnitType   -> \_ u -> sout "kotlin.Unit"
      KtBoolType   -> \_ b -> sout $ if b then "true" else "false"

readFuns :: Map Name KtFun0
readFuns = fromList
  [ "readLine()!!.toInt@" `to` KtFun0 (\_ -> read @Int <$> getLine)
  , "readLine()!!.toDouble@" `to` KtFun0 (\_ -> read @Double <$> getLine)
  , "readLine@" `to` KtFun0 (\_ -> getLine)
  ]

to :: a -> b -> (a, b)
to = (,)

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
    case getFun0 scope "main" KtUnitType of
      Nothing -> fail "Execution error: `main : () -> Unit` function not defined!"
      Just f -> f scope
    where
      iterator
        :: Interpret (KtFunData a) -> Map String a -> Map String a
      iterator iFun funs =
        let (name, types, fun) = interpret iFun
          in insert (fun2key name types) fun funs

  ktFun0
    :: Name
    -> KtAnyType
    -> [Interpret KtCommand]
    -> Interpret (KtFunData KtFun0)
  ktFun0 name raType@(KtAnyType (rType :: KtType rT)) cmds =
    Interpret (name, [], fun0)
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
    Interpret (name, [aaType], fun1)
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
    Interpret (name, [a1aType, a2aType], fun2)
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
  
  ktValueCommand :: Interpret KtAnyValue -> Interpret KtCommand
  ktValueCommand iv = Interpret . KtCommandStep $ \scope -> do
    runAnyValue (interpret iv) scope
    return scope 

  ktCallFun0 :: Name -> Interpret KtAnyValue
  ktCallFun0 name = Interpret $ \scope ->
    findFun0 scope name


  ktCallFun1 :: Name -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktCallFun1 name ia = Interpret $ \scope ->
    let ha = interpret ia scope
     in case findType ha of
          KtAnyType aType ->
            let ioa = strongCastAnyType aType ha
             in findFun1 scope name aType ioa

  ktCallFun2 :: Name -> Interpret KtAnyValue -> Interpret KtAnyValue -> Interpret KtAnyValue
  ktCallFun2 name ia1 ia2 = Interpret $ \scope ->
    let ha1 = interpret ia1 scope
        ha2 = interpret ia2 scope
     in case (findType ha1, findType ha2) of
          (KtAnyType a1Type, KtAnyType a2Type) ->
            let ioa1 = strongCastAnyType a1Type ha1
                ioa2 = strongCastAnyType a2Type ha2
             in findFun2 scope name a1Type ioa1 a2Type ioa2

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
interpretConstant a = Interpret $ \_ -> HiddenIO $ return a

mkFunBody :: (Typeable r) => KtType r -> [Interpret KtCommand] -> KtScope -> IO r
mkFunBody (rType :: KtType rT) cmds funScope = do
  mbResult <- foldCommands funScope rType $ interpret <$> cmds
  case mbResult of
    Nothing -> case eqT @rT @() of
      Nothing   -> fail "Missing return"
      Just Refl -> return ()
    Just r  -> return r

foldCommands :: Typeable a => KtScope -> KtType a -> [KtCommand] -> IO (Maybe a)
foldCommands initScope (ktType :: KtType a) cmds = foldCommands' initScope cmds Nothing
  where
    foldCommands' :: KtScope -> [KtCommand] -> Maybe a -> IO (Maybe a)
    foldCommands' scope [] mbV = return mbV
    foldCommands' scope (cmd:cmds) Nothing =
      case cmd of
        KtCommandReturn underScope ->
          case underScope scope of
            HiddenIO (ioR :: IO r) ->
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
            HiddenIO (_ :: IO r) ->
              case eqT @a @r of
                Nothing   -> fail "Incorrect type of return statement"
                Just Refl -> foldCommands' scope cmds jV
        KtCommandStep ioS ->
          foldCommands' scope cmds jV
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds jV >> foldCommands' scope cmds jV

getFun0 :: Typeable r => KtScope -> Name -> KtType r -> Maybe (KtScope -> IO r)
getFun0 KtScope { sFun0 = funs } name (rType :: KtType rT) = do
  KtFun0 (fun0 :: KtScope -> IO r) <- funs !? (fun2key name [])
  Refl <- eqT @r @rT
  return fun0

getFun1
  :: (Typeable a, Typeable r)
  => KtScope
  -> Name
  -> KtType a
  -> KtType r
  -> Maybe (KtScope -> a -> IO r)
getFun1 KtScope { sFun1 = funs } name (aType :: KtType aT) (rType :: KtType rT) = do
  KtFun1 (fun1 :: KtScope -> a -> IO r) <-
    funs !? (fun2key name [KtAnyType aType])
  Refl <- eqT @a @aT
  Refl <- eqT @r @rT
  return fun1

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
    funs !? (fun2key name [KtAnyType a1Type, KtAnyType a2Type])
  Refl <- eqT @a1 @a1T
  Refl <- eqT @a2 @a2T
  Refl <- eqT @r  @rT
  return fun2

findFun :: (KtAnyType -> Maybe fun) -> Maybe fun
findFun tryFind = getFirst . mconcat $ First . tryFind <$> allKtTypes
  where
    allKtTypes :: [KtAnyType]
    allKtTypes =
      [ KtAnyType KtIntType
      , KtAnyType KtDoubleType
      , KtAnyType KtStringType
      , KtAnyType KtUnitType
      , KtAnyType KtBoolType
      ]

findFun0 :: KtScope -> Name -> HiddenIO
findFun0 scope name =
  let mbFun = findFun $ \(KtAnyType rType) ->
        (\f -> HiddenIO $ f scope) <$> getFun0 scope name rType
   in case mbFun of
        Nothing -> error "No such function"
        Just f  -> f

findFun1 :: (Typeable a) => KtScope -> Name -> KtType a -> IO a -> HiddenIO
findFun1 scope name (aType :: KtType a) ioa =
  let mbFun = findFun $ \(KtAnyType rType) ->
        (\f -> HiddenIO $ ioa >>= f scope) <$> getFun1 scope name aType rType
   in case mbFun of
        Nothing -> error "No such function"
        Just f  -> f

findFun2
  :: (Typeable a1, Typeable a2)
  => KtScope -> Name -> KtType a1 -> IO a1 -> KtType a2 -> IO a2 -> HiddenIO
findFun2 scope name (a1Type :: KtType a1) ioa1 (a2Type :: KtType a2) ioa2 =
  let mbFun = findFun $ \(KtAnyType rType) ->
        (\f ->
          HiddenIO $ do 
            a1 <- ioa1 
            a2 <- ioa2
            f scope a1 a2
        ) <$> getFun2 scope name a1Type a2Type rType
   in case mbFun of
        Nothing -> error "No such function"
        Just f  -> f

castAnyType :: (Typeable a) => KtType a -> HiddenIO -> Maybe (IO a)
castAnyType (_ :: KtType aT) (HiddenIO (r :: IO anyT)) = do
  Refl <- eqT @aT @anyT
  pure r

runAnyValue :: KtAnyValue -> KtScope -> IO ()
runAnyValue ha scope =
  case ha scope of
    HiddenIO ioa -> ioa >> return ()

strongCastAnyType :: (Typeable a) => KtType a -> HiddenIO -> IO a
strongCastAnyType aType ha =
  case castAnyType aType ha of
    Nothing -> error "Cannot unwrap hidden IO"
    Just io -> io

findType :: HiddenIO -> KtAnyType
findType (HiddenIO (a :: IO aT)) =
  let tryMatch = ( eqT @aT @Int
                 , eqT @aT @Double
                 , eqT @aT @String
                 , eqT @aT @Bool
                 , eqT @aT @()
                 )
   in case tryMatch of
        (Just Refl, _, _, _, _) -> KtAnyType KtIntType
        (_, Just Refl, _, _, _) -> KtAnyType KtDoubleType
        (_, _, Just Refl, _, _) -> KtAnyType KtStringType
        (_, _, _, Just Refl, _) -> KtAnyType KtBoolType
        (_, _, _, _, Just Refl) -> KtAnyType KtUnitType
        _                       -> error "cannot deduce type"


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
interpretUnoOp predator iv = Interpret $ \scope ->
  let hio = interpret iv scope in
  case (castAnyType KtIntType hio, castAnyType KtDoubleType hio, castAnyType KtBoolType hio) of
      (Just iov, _, _) -> HiddenIO $ uOnInt    predator <$> iov
      (_, Just iov, _) -> HiddenIO $ uOnDouble predator <$> iov
      (_, _, Just iov) -> HiddenIO $ uOnBool   predator <$> iov
      _                -> error "Invalid type of argument"

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
interpretBinOp predator il ir = Interpret $ \scope ->
  let al = interpret il scope
      ar = interpret ir scope
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
        (Just iol, _, _, _, _, Just ior, _, _, _, _) -> HiddenIO $ liftM2 (bOnInt    predator) iol ior
        (_, Just iol, _, _, _, _, Just ior, _, _, _) -> HiddenIO $ liftM2 (bOnDouble predator) iol ior
        (_, _, Just iol, _, _, _, _, Just ior, _, _) -> HiddenIO $ liftM2 (bOnBool   predator) iol ior
        (_, _, _, Just iol, _, _, _, _, Just ior, _) -> HiddenIO $ liftM2 (bOnString predator) iol ior
        (_, _, _, _, Just iol, _, _, _, _, Just ior) -> HiddenIO $ liftM2 (bOnUnit   predator) iol ior
        (Just iol, _, _, _, _, _, Just ior, _, _, _) ->
          if bCanCast predator
          then HiddenIO $ liftM2 (bOnDouble predator) (int2Double <$> iol) ior
          else binError
        (_, Just iol, _, _, _, Just ior, _, _, _, _) ->
          if bCanCast predator
          then HiddenIO $ liftM2 (bOnDouble predator) iol (int2Double <$> ior)
          else binError
        _ -> binError
    where
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
