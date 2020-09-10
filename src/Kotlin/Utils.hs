{-# LANGUAGE GADTs #-}

module Kotlin.Utils
  ( fun2key
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