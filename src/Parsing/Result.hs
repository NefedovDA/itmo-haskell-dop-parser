{-# LANGUAGE InstanceSigs #-}

module Parsing.Result
  ( Result(..)

  , catchE
  , failE
  , returnE
  , thenE

  , parseError
  ) where

import Parsing.Token (Token)

-- | Data for handling result of parsing.
data Result a
  = Ok a
  | Failed String
  deriving (Show, Eq)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f r = case r of
    Ok a     -> Ok (f a)
    Failed b -> Failed b

-- | As @(>>=)@ for monad.
thenE :: Result a -> (a -> Result b) -> Result b
m `thenE` k = case m of
  Ok a     -> k a
  Failed e -> Failed e

-- | As @return@ for monad.
returnE :: a -> Result a
returnE = Ok

-- | Create error with message.
failE :: String -> Result a
failE = Failed

-- | Catch error.
catchE :: Result a -> (String -> Result a) -> Result a
catchE m k = case m of
  Ok a     -> Ok a
  Failed e -> k e

-- | Parse error from given tokens.
parseError :: [Token] -> Result a
parseError tokens = failE $ errorMessage tokens

-- | Return message by tail of the tokens.
errorMessage :: [Token] -> String
errorMessage tokens =
  "Parse error on token: " ++
    case tokens of
      []    -> "Sudden end of input"
      (t:_) -> show t
