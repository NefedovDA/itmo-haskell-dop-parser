{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module Parsing.Expression
  ( Kotlin (..)
  , KotlinPsi(..)

  , ToS (..)
  , Interpret(..)

  , KtFile
  , KtFun0
  , KtFun1
  , KtFun2
  , KtFun0Unit
  , Name
  , Unit

  , transform
  )
where

import           Data.List (intercalate)
import           Data.Map  (Map)
import qualified Data.Map  as M

data Scope = Scope { sFun0Unit :: Map Name (Scope -> IO Unit) }

type KtFile = IO ()

type KtFun0       r = (Name, Scope -> IO r)
type KtFun1 a1    r = (Name, Scope -> a1 -> IO r)
type KtFun2 a1 a2 r = (Name, Scope -> a1 -> a2 -> IO r)

type KtFun0Unit = KtFun0 Unit

type Name = String

type Unit = ()

type KtCall = Scope -> IO Value
  
data Value
  = ValueInt Int
  | ValueDouble Double
  | ValueString String
  | ValueBool Bool
  | ValueUnit Unit

class Kotlin expr where
  ktFile     :: [expr KtFun0Unit] -> expr KtFile
  ktFun0Unit :: Name -> [expr KtCall] -> expr KtFun0Unit
  ktCallFun0 :: Name -> expr KtCall

data KotlinPsi a where
  KtPsiFile     :: [KotlinPsi KtFun0Unit] -> KotlinPsi KtFile
  KtPsiFun0Unit :: Name -> [KotlinPsi KtCall] -> KotlinPsi KtFun0Unit
  KtPsiCallFun0 :: Name -> KotlinPsi KtCall

newtype ToS a = ToS { toString :: String }
  deriving (Show, Semigroup)

instance Kotlin ToS where
  ktFile :: [ToS KtFun0Unit] -> ToS KtFile
  ktFile fns = ToS $ intercalate "\n" $ toString <$> fns

  ktFun0Unit :: Name -> [ToS KtCall] -> ToS KtFun0Unit
  ktFun0Unit name body = ToS $ "fun " ++ name ++ "(): Unit {\n" ++ (intercalate "\n" $ toString <$> body) ++ "}"
  
  ktCallFun0 :: Name -> ToS KtCall
  ktCallFun0 name = ToS $ name ++ "();"

newtype Interpret a = Interpret { interpret :: a }

instance Kotlin Interpret where
  ktFile :: [Interpret KtFun0Unit] -> Interpret KtFile
  ktFile fns = Interpret $ do
    let fullScope = foldr iterator emptyScope fns
    callFun0 fullScope "main"
    where
      iterator :: Interpret KtFun0Unit -> Scope -> Scope
      iterator fn scp = Scope $ uncurry M.insert (interpret fn) $ sFun0Unit scp

  ktFun0Unit :: Name -> [Interpret KtCall] -> Interpret KtFun0Unit
  ktFun0Unit name body = Interpret $ (name, fn)
    where
      fn :: Scope -> IO ()
      fn scope = do
        putStrLn name
        mapM_ (flip interpret scope) body
  
  ktCallFun0 :: Name -> Interpret KtCall
  ktCallFun0 name = Interpret $ \scope -> ValueUnit <$> callFun0 scope name

callFun0 :: Scope -> Name -> IO ()
callFun0 scope name = case sFun0Unit scope M.!? name of
   Nothing     -> fail $ "Execution error: `" ++ name ++ "` function not defined!"
   Just action -> action scope

transform :: Kotlin expr => KotlinPsi a -> expr a
transform a = case a of
  KtPsiFile list  -> ktFile (transform <$> list)
  KtPsiFun0Unit n list -> ktFun0Unit n (transform <$> list)
  KtPsiCallFun0 n -> ktCallFun0 n

emptyScope :: Scope
emptyScope = Scope M.empty
