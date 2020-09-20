{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parsing.Utils
  ( addBranch

  , checkedInt
  , checkedDouble
  , updatedString

  , defaultReturn
  ) where

import Data.List (intercalate)
import Data.Set  (Set, insert, member, empty)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Kotlin.Dsl
import Kotlin.Utils
import Parsing.KotlinPsi
import Parsing.Result
import Control.Exception (throwIO)

addBranch
  :: (KotlinPsi (KtValue c), [KotlinPsi (KtCommand c)])
  -> KotlinPsi (KtCommand c)
  -> KotlinPsi (KtCommand c)
addBranch branch = \case
  KtPsiIf branches elseBranch -> KtPsiIf (branch : branches) elseBranch
  _ -> error "LOGIC ERROR (addBranch cannot be call not on KtPsiIf)"

checkedInt :: String -> Result (KotlinPsi (KtValue IO))
checkedInt str = case readMaybe @Int str of
  Nothing -> failE $ "Illegal Int constant: " ++ str
  Just i  -> returnE $ KtPsiInt i

checkedDouble :: String -> Result (KotlinPsi (KtValue IO))
checkedDouble str = case readMaybe @Double str of
  Nothing -> failE $ "Illegal Double constant: " ++ str
  Just d  -> returnE $ KtPsiDouble d

updatedString :: String -> KotlinPsi (KtValue IO)
updatedString (_:str) = KtPsiString $ dropLast str

dropLast :: [a] -> [a]
dropLast xs = f xs (tail xs)
    where
      f :: [a] -> [a] -> [a] 
      f (x:xs) (y:ys) = x : f xs ys
      f _ _ = []

defaultReturn :: KotlinPsi (KtCommand IO)
defaultReturn = KtPsiReturn $ KtPsiUnit ()

instance Console IO where
  consolePrint :: String -> IO ()
  consolePrint s = putStr s >> hFlush stdout
  
  consolePrintln  :: String -> IO ()
  consolePrintln = putStrLn
  
  consoleReadLine :: IO String
  consoleReadLine = readLn
