{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpreter
  ( Interpret(..)
  ) where

import Data.Map      (Map, empty, insert, (!?))
import Data.Typeable ((:~:)(..), eqT)

import Kotlin.Dsl

newtype Interpret a = Interpret { interpret :: a }

instance Kotlin Interpret where
  ktFile :: FunDecl Interpret -> Interpret KtFile
  ktFile funDec = Interpret $ do
    let scope = Scope
          { sFun0 = foldr iterator empty $ fdFun0 funDec
          , sFun1 = foldr iterator empty $ fdFun1 funDec
          , sFun2 = foldr iterator empty $ fdFun2 funDec
          }

    let (mbMain :: Maybe (Scope -> IO ())) = do
          (KtFun0 (f :: Scope -> IO t)) <- sFun0 scope !? "main"
          Refl <- eqT @t @()
          pure f

    case mbMain of
      Nothing -> fail "Execution error: `main : () -> Unit` function not defined!"
      Just f  -> f scope
    where
      iterator :: Interpret (Name, a) -> Map Name a -> Map Name a
      iterator fn fns = uncurry insert (interpret fn) fns

  ktFun0 :: Name -> KtType -> Interpret KtFun0Data
  ktFun0 name rType = Interpret (name, undefined)

  ktFun1 :: Name -> KtFunArg -> KtType -> Interpret KtFun1Data
  ktFun1 name arg rType = Interpret (name, undefined)

  ktFun2 :: Name -> KtFunArg -> KtFunArg -> KtType -> Interpret KtFun2Data
  ktFun2 name arg1 arg2 rType = Interpret (name, undefined)
  
  ktReturn :: Interpret r -> Interpret KtCmd
  ktReturn r = Interpret $ KtCmdReturn $ \scope -> return $ interpret r

  ktInt :: Int -> Interpret KtInt
  ktInt = interpretConstant

  ktDouble :: Double -> Interpret KtDouble
  ktDouble = interpretConstant

  ktString :: String -> Interpret KtString
  ktString = interpretConstant

  ktBool :: Bool -> Interpret KtBool
  ktBool = interpretConstant

  ktUnit :: Interpret KtUnit
  ktUnit = interpretConstant ()

interpretConstant :: a -> Interpret (IO a)
interpretConstant a = Interpret $ return a