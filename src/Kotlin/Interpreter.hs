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
          { sFun0 = empty
          , sFun1 = empty
          , sFun2 = empty
          
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

  ktFun0 :: Name -> KtAnyType -> Interpret (KtFunData KtFun0)
  ktFun0 name rType = Interpret undefined

  ktFun1 :: Name -> KtFunArg -> KtAnyType -> Interpret (KtFunData KtFun1)
  ktFun1 name arg rType = Interpret undefined

  ktFun2 :: Name -> KtFunArg -> KtFunArg -> KtAnyType -> Interpret (KtFunData KtFun2)
  ktFun2 name arg1 arg2 rType = Interpret undefined
  
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

interpretConstant :: Typeable a => a -> Interpret KtValue
interpretConstant a = Interpret . KtValue $ \_ -> return a

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