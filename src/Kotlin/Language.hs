{-# LANGUAGE RankNTypes #-}

-- |
-- This module help to write more idiomatic declaration of Kotlin eDSL.
-- 
-- Example of usage:
-- @
-- code :: (Kotlin expr, Console c) => expr (KtFile c)
-- code = ktFile $
--   [ fun "f" ["i" .: _Int] _Int
--       [ ktReturn $ get "i"
--       ]
--   , fun "main" [] _Unit
--       [ val "s" _String .=
--           ktString "left>" @+@ ktString "<right"
--       , "println" .!# [get "s"]
--       , var "i" _Int .= ktInt 1
--       , "println" .!# [get "i"]
--       , onTop $ "f".![ktInt 1] @+@ ktInt 1
--       , "i" .=# get "i" @+@ get "i" @+@ get "i"
--       , "println" .!# [get "i"]
--       , for ("i" `_in` ktInt 1 ..# ktInt 10)
--           [ "print" [get "i"]
--           , "print" .!# [ktString " is "]
--           , _if (ktReadVariable "i" @/@ ktInt 2 @*@ ktInt 2 @==@ ktReadVariable "i")
--               [ "println" .!# [ktString "even"]
--               ]
--             $ _else_if (ktBool False)
--               []
--             $ _else_
--               [ "println" .!# [ktString "odd"]
--               ]
--           ]
--       ]
--   ]
-- @
module Kotlin.Language
  ( module Kotlin.Dsl
  
  , (.!)
  , (.!#)
  , (..#)
  , (.:)
  , (.=)
  , (.=#)
  
  , _Bool
  , _Double
  , _Int
  , _String
  , _Unit

  , fun

  , get
  , val
  , var

  , _else_
  , _else_if
  , _if

  , _in
  , for

  , onTop
  ) where

import Kotlin.Dsl
import Data.Bifunctor (first)

-- Operators --

-- | Assign name of argument with its type.
--
-- In Kotlin:
-- @fun f(a: Int, b: String)@
--
-- In Kotlin eDsl:
-- @fun "f" ["a" .: _Int, "b" .: _String]@
infix 1 .:
(.:) :: Name -> KtAnyType -> KtFunArg
(.:) = (,)

-- | Representation of @=@ operator from Kotlin for initialising of variables.
--
-- In Kotlin:
-- @val a : Int = 1@
--
-- In Kotlin eDSL:
-- @val "a" _Int .= ktInt 1
infix 1 .=
(.=)
  :: (expr (KtValue c) -> expr (KtCommand c))
  -> expr (KtValue c)
  -> expr (KtCommand c)
constr .= value = constr value

-- | Version of @.=@ for reassign variables.
--
-- In Kotlin:
-- @a = 1@
--
-- In Kotlin eDSL:
-- @"a" .=# ktInt 1@
infix 1 .=#
(.=#)
  :: (Kotlin expr, Console c)
  => Name
  -> expr (KtValue c)
  -> expr (KtCommand c)
(.=#) = ktSetVariable

-- | Call of function.
--
-- In Kotlin:
-- @f(10) + 1@
--
-- In Kotlin eDSL:
-- @"f".![ktInt 10] \@+\@ ktInt 1@
infix 9 .!
(.!)
  :: (Kotlin expr, Console c)
  => Name -> [expr (KtValue c)] -> expr (KtValue c)
(.!)= ktCallFun

-- | Call of function on top level.
--
-- In Kotlin:
-- @
-- { f(10); }
-- @
--
-- In Kotlin eDSL:
-- @[ "f".!#[ktInt 10] ]@
infix 9 .!#
(.!#)
  :: (Kotlin expr, Console c)
  => Name -> [expr (KtValue c)] -> expr (KtCommand c)
name .!# args = ktValueCommand $ ktCallFun name args

-- | Range operator.
--
-- In Kotlin:
-- @for (i in 1..10)@
--
-- In Kotlin eDSL:
-- @for ("i" `_in` ktInt 1 ..# ktInt 10)@
infix 1 ..#
(..#) :: expr (KtValue c) -> expr (KtValue c) -> (expr (KtValue c), expr (KtValue c))
(..#) = (,)

-- Types --

-- | Bindings of Kotlin types.
_Int, _Double, _String, _Bool, _Unit :: KtAnyType
_Int    = KtAnyType KtIntType
_Double = KtAnyType KtDoubleType
_String = KtAnyType KtStringType
_Bool   = KtAnyType KtBoolType
_Unit   = KtAnyType KtUnitType

-- Function definition --

-- | Binding 'ktFun' to Kotlin key word.
--
-- In Kotlin:
-- @fun f(a: Int, b: String)@
--
-- In Kotlin eDsl:
-- @fun "f" ["a" .: _Int, "b" .: _String]@
fun
  :: (Kotlin expr, Console c)
  => Name
  -> [KtFunArg]
  -> KtAnyType
  -> [expr (KtCommand c)]
  -> expr (KtFunData c)
fun = ktFun

-- Variables --

-- | Binding 'ktInitVariable' to Kotlin key word.
--
-- In Kotlin:
-- @val a : Int = 1@
--
-- In Kotlin eDSL:
-- @val "a" _Int .= ktInt 1
val
  :: (Kotlin expr, Console c)
  => Name
  -> KtAnyType
  -> expr (KtValue c)
  -> expr (KtCommand c)
val = ktInitVariable True

-- | Binding 'ktInitVariable' to Kotlin key word.
--
-- In Kotlin:
-- @var a : Int = 1@
--
-- In Kotlin eDSL:
-- @var "a" _Int .= ktInt 1
var
  :: (Kotlin expr, Console c)
  => Name
  -> KtAnyType
  -> expr (KtValue c)
  -> expr (KtCommand c)
var = ktInitVariable False

-- | Short version of 'ktReadVariable'.
--
-- In Kotlin:
-- @f(i)@
--
-- In Kotlin eDSL:
-- @"f".![get "i"]@
get
  :: (Kotlin expr, Console c)
  => Name
  -> expr (KtValue c)
get = ktReadVariable

-- if --

-- | Helper type binding.
type IfData expr c =
  ( [(expr (KtValue c), [expr (KtCommand c)])]
  , [expr (KtCommand c)]
  )

-- | Representation of Kotlin @if@ key word.
--
-- In Kotlin:
-- @
-- if (true) { }
-- else { }
-- @
--
-- In Kotlin eDSL:
-- @
-- _if (ktBool True) []
-- $ _else_ []
-- @
_if
  :: (Kotlin expr, Console c)
  => expr (KtValue c) -> [expr (KtCommand c)] -> IfData expr c -> expr (KtCommand c)
_if condition body = uncurry ktIf . _else_if condition body

-- | Representation of Kotlin @if else@ key words.
--
-- In Kotlin:
-- @
-- if (false) { }
-- else if (true) { }
-- @
--
-- In Kotlin eDSL:
-- @
-- _if (ktBool False) []
-- $ _else_if (ktBool True) []
-- $ _else []
-- @
_else_if
 :: (Kotlin expr, Console c)
 => expr (KtValue c) -> [expr (KtCommand c)] -> IfData expr c -> IfData expr c
_else_if condition body = first ((condition, body) :)

-- | Representation of Kotlin @else@ key word.
--
-- In Kotlin:
-- @
-- if (true) { }
-- else { }
-- @
--
-- In Kotlin eDSL:
-- @
-- _if (ktBool True) []
-- $ _else_ []
-- @
_else_
 :: (Kotlin expr, Console c)
 => [expr (KtCommand c)] -> IfData expr c
_else_ elseBranch = ([], elseBranch)

-- For --

-- | Helper type binding.
type ForRangeType expr c =
  (expr (KtValue c), expr (KtValue c))

-- | Representation of Kotlin @for@ key word.
--
-- In Kotlin:
-- @for (i in 1..10)@
--
-- In Kotlin eDSL:
-- @for ("i" `_in` ktInt 1 ..# ktInt 10)@
for
  :: (Kotlin expr, Console c)
  => (Name, ForRangeType expr c) -> [expr (KtCommand c)] -> expr (KtCommand c)
for (name, (from, to)) = ktFor name from to


-- | Representation of Kotlin @in@ key word.
--
-- In Kotlin:
-- @for (i in 1..10)@
--
-- In Kotlin eDSL:
-- @for ("i" `_in` ktInt 1 ..# ktInt 10)@
infix 0 `_in`
_in
 :: (Kotlin expr, Console c)
 => Name -> ForRangeType expr c -> (Name, ForRangeType expr c)
_in = (,)

-- Other --

-- | Short version of 'ktReadVariable'.
--
-- In Kotlin:
-- @
-- { 10; }
-- @
--
-- In Kotlin eDSL:
-- @[ onTop $ ktInt 10 ]@
onTop
  :: (Kotlin expr, Console c)
  => expr (KtValue c)
  -> expr (KtCommand c)
onTop = ktValueCommand
