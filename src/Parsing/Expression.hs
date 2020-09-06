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

class Kotlin expr where
  ktFile     :: [expr KtFun0Unit] -> expr KtFile
  ktFun0Unit :: Name -> expr KtFun0Unit

data KotlinPsi a where
  KtPsiFile     :: [KotlinPsi KtFun0Unit] -> KotlinPsi KtFile
  KtPsiFun0Unit :: Name -> KotlinPsi KtFun0Unit

newtype ToS a = ToS { toString :: String }
  deriving (Show, Semigroup)

instance Kotlin ToS where
  ktFile :: [ToS KtFun0Unit] -> ToS KtFile
  ktFile fns = ToS $ intercalate "\n" $ toString <$> fns

  ktFun0Unit :: Name -> ToS KtFun0Unit
  ktFun0Unit name = ToS $ "fun " ++ name ++ "(): Unit {}"

newtype Interpret a = Interpret { interpret :: a }

instance Kotlin Interpret where
  ktFile :: [Interpret KtFun0Unit] -> Interpret KtFile
  ktFile fns = Interpret $ do
    let fullScope = foldr iterator emptyScope fns
    callMain fullScope
    where
      iterator :: Interpret KtFun0Unit -> Scope -> Scope
      iterator fn scp = Scope $ uncurry M.insert (interpret fn) $ sFun0Unit scp
      
      callMain :: Scope -> IO ()
      callMain scope = case sFun0Unit scope M.!? "main" of
        Nothing     -> putStrLn "Executable error: `main` function not defined!"
        Just action -> action scope

  ktFun0Unit :: Name -> Interpret KtFun0Unit
  ktFun0Unit name = Interpret $ (name, \_ -> return ())

transform :: Kotlin expr => KotlinPsi a -> expr a
transform a = case a of
  KtPsiFile list  -> ktFile (map transform list)
  KtPsiFun0Unit n -> ktFun0Unit n

emptyScope :: Scope
emptyScope = Scope M.empty
