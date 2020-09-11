{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpreter
  ( Interpret(..)
  ) where

import Data.Map      (Map, insert, empty, (!?))
import Data.Typeable (Typeable, (:~:)(..), eqT)

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
  
  ktReturn :: Interpret KtValue -> Interpret KtCommand
  ktReturn result = Interpret undefined

  ktInt :: Int -> Interpret KtValue
  ktInt = interpretConstant

  ktDouble :: Double -> Interpret KtValue
  ktDouble = interpretConstant

  ktString :: String -> Interpret KtValue
  ktString = interpretConstant

  ktBool :: Bool -> Interpret KtValue
  ktBool = interpretConstant

  ktUnit :: () -> Interpret KtValue
  ktUnit = interpretConstant

interpretConstant ::( Typeable a) => a -> Interpret KtValue
interpretConstant a = Interpret . KtValue $ \_ -> return a

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
        KtCommandReturn (KtValue (ioR :: KtScope -> IO r)) ->
          case eqT @a @r of
            Nothing   -> fail ""
            Just Refl -> ioR scope >>= \r -> foldCommands' scope cmds $ Just r
        KtCommandStep ioS ->
          ioS scope >>= \(newScope, _) -> foldCommands' newScope cmds Nothing
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds Nothing >>= \mbR -> foldCommands' scope cmds mbR
    foldCommands' scope (cmd:cmds) jV@(Just v) =
      case cmd of
        KtCommandReturn (KtValue (ioR :: KtScope -> IO r)) ->
          case eqT @a @r of
            Nothing   -> fail ""
            Just Refl -> foldCommands' scope cmds jV
        KtCommandStep ioS ->
          foldCommands' scope cmds jV
        KtCommandBlock blockCmds ->
          foldCommands' scope blockCmds jV >> foldCommands' scope cmds jV
