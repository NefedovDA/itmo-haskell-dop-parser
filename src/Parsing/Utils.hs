{-# LANGUAGE GADTs            #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Parsing.Utils
  ( addBranch

  , checkedInt
  , checkedDouble
  , updatedString

  , defaultReturn
  ) where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Parsing.KotlinPsi
import Parsing.Result

-- | Add branch to the if command.
addBranch
  :: (KotlinPsi (KtValue c), [KotlinPsi (KtCommand c)])
  -> KotlinPsi (KtCommand c)
  -> KotlinPsi (KtCommand c)
addBranch branch = \case
  KtPsiIf branches elseBranch -> KtPsiIf (branch : branches) elseBranch
  _ -> error "LOGIC ERROR: addBranch cannot be call not on KtPsiIf"

-- | Try cast int token to @Int@
checkedInt :: String -> Result (KotlinPsi (KtValue IO))
checkedInt str = case readMaybe @Int str of
  Nothing -> failE $ "Illegal Int constant: " ++ str
  Just i  -> returnE $ KtPsiInt i

-- | Try cast double token to @Double@
checkedDouble :: String -> Result (KotlinPsi (KtValue IO))
checkedDouble str = case readMaybe @Double str of
  Nothing -> failE $ "Illegal Double constant: " ++ str
  Just d  -> returnE $ KtPsiDouble d

-- | Trim `"` for string token value.
updatedString :: String -> KotlinPsi (KtValue IO)
updatedString (_:str) = KtPsiString $ dropLast str
  where
    dropLast :: [a] -> [a]
    dropLast xs = go xs (tail xs)
      where
        go :: [a] -> [a] -> [a]
        go (x:xs) (y:ys) = x : go xs ys
        go _ _ = []

-- | Return command for `return;` statement.
defaultReturn :: KotlinPsi (KtCommand IO)
defaultReturn = KtPsiReturn $ KtPsiUnit ()

instance Console IO where
  consolePrint :: String -> IO ()
  consolePrint s = putStr s >> hFlush stdout

  consolePrintln  :: String -> IO ()
  consolePrintln = putStrLn

  consoleReadLine :: IO String
  consoleReadLine = readLn
