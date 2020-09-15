{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Kotlin.Utils
  ( fun2key
  , to
  , fromJust
  ) where

import Kotlin.Dsl

fun2key :: Name -> [KtAnyType] -> String
fun2key name types = name ++ "@" ++ foldMap type2id types
  where
    type2id :: KtAnyType -> String
    type2id (KtAnyType ktType) =
      case ktType of
        KtIntType    -> "i"
        KtDoubleType -> "d"
        KtStringType -> "s"
        KtUnitType   -> "u"
        KtBoolType   -> "b"

to :: a -> b -> (a, b)
to = (,)

fromJust :: String -> Maybe a -> a
fromJust msg = \case
  Just a  -> a
  Nothing -> error msg
