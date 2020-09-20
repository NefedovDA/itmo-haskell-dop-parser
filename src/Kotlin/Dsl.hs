{-# LANGUAGE GADTs #-}

module Kotlin.Dsl
  ( Kotlin(..)
  , Console(..)

  , KtDeclarations
  , KtScope(..)

  , KtFile

  , KtFun

  , KtFunArg
  , KtFunData
  , KtFunKey

  , KtVariableInfo

  , KtValue

  , Name

  , KtCommand(..)

  , HiddenIO(..)

  , KtAnyType(..)
  , KtType(..)
  ) where

import Data.Map      (Map)
import Data.Typeable (Typeable)

-- | Type class for creation Kotlin eDSL.
-- Support basic functionality of the language:
--   * Top-level functions.
--   * @val@ and @var@ variables, but not support top-level definition.
--   * Print and read values from console.
--   * @for@ loop.
--   * @if-else@ statement, not an expression.
--   * Primitive types: @Int@, @Double@, @String@, @Bool@ and @Unit@.
-- 
-- [@expr@] Type that will work with Kotlin code tree.
-- [@c@]    Console monad to support non-pure behavior.
class Kotlin expr where
  -- | Define file structure.
  ktFile
    :: (Console c)
    => KtDeclarations expr c  -- ^ List of the functions of the file.
    -> expr (KtFile c)        -- ^ Wrapped file.

  -- | Define function structure.
  ktFun
    :: (Console c)
    => Name                  -- ^ Name of the function.
    -> [KtFunArg]            -- ^ List of function arguments.
    -> KtAnyType             -- ^ Return type of the function.
    -> [expr (KtCommand c)]  -- ^ Body - list of the commands.
    -> expr (KtFunData c)    -- ^ Wrapped function.

  -- | Command that init new variable.
  ktInitVariable
    :: (Console c)
    => Bool                -- ^ Set if variable is constant.
    -> Name                -- ^ A variable name.
    -> KtAnyType           -- ^ A type of variable.
    -> expr (KtValue c)    -- ^ A value with which variable will be initiated.
    -> expr (KtCommand c)  -- ^ Wrapped command.

  -- | Command that reassign variable.
  ktSetVariable
    :: (Console c)
    => Name                -- ^ A name of the variable.
    -> expr (KtValue c)    -- ^ A value with which variable will be assigned.
    -> expr (KtCommand c)  -- ^ Wrapped command.

  -- | Return command.
  -- Interrupts the thread of execution and returns the value from the function.
  ktReturn
    :: (Console c)
    => expr (KtValue c)    -- ^ Value that will be returned.
    -> expr (KtCommand c)  -- ^ Wrapped command.

  -- | Command that evaluate given value.
  ktValueCommand
    :: (Console c)
    => expr (KtValue c)   -- ^ Target value, that will be evaluated.
    -> expr (KtCommand c) -- ^ Wrapped command.

  -- | Command that run the for loop.
  ktFor
    :: (Console c)
    => Name
    -> expr (KtValue c)      -- ^ Left bound of the loop range of type Int.
    -> expr (KtValue c)      -- ^ Right bound of the loop range of type Int.
    -> [expr (KtCommand c)]  -- ^ Body - list of the commands.
    -> expr (KtCommand c)    -- ^ Wrapped command.

  -- | Command that run if decider.
  ktIf
    :: (Console c)
    => -- ^ List of the if branches - Bool value to the command list.
       [(expr (KtValue c), [expr (KtCommand c)])]
    -> [expr (KtCommand c)]  -- ^ Else branch - list of the commands.
    -> expr (KtCommand c)    -- ^ Wrapped command.

  -- | Call the function operation.
  ktCallFun
    :: (Console c)
    => Name                -- ^ Name of the function to call.
    -> [expr (KtValue c)]  -- ^ List of arguments.
    -> expr (KtValue c)    -- ^ Wrapped result of the operation.

  -- | Reading value of the variable operation.
  ktReadVariable
    :: (Console c)
    => Name              -- ^ Name of the variable to read.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Unary minus operation.
  -- @val x = -y@
  ktNegate
    :: (Console c)
    => expr (KtValue c)  -- ^ Value that should be negated.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Logic not operation.
  -- @if (!x) {}@
  ktNot
    :: (Console c)
    => expr (KtValue c)  -- ^ Value that should be inverted.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary mult operation.
  -- @val x = 1 * 2@
  infixl 7 @*@
  (@*@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary div operation.
  -- @
  --  val x = 1 / 2    // x = 0
  --  val y = 1.0 / 2  // y = 0.5
  -- @
  infixl 7 @/@
  (@/@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary plus operation.
  -- @
  --  val x = 1 + 2
  --  val s = "aa" + "bb"
  -- @
  infixl 6 @+@
  (@+@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary diff operation.
  -- @val x = 5 - 3@
  infixl 6 @-@
  (@-@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary great operation.
  -- @if(x > y) {}@
  infixl 5 @>@
  (@>@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary great or equal operation.
  -- @if(x >= y) {}@
  infixl 5 @>=@
  (@>=@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary less operation.
  -- @if(x < y) {}@
  infixl 5 @<@
  (@<@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary less or equal operation.
  -- @if(x <= y) {}@
  infixl 5 @<=@
  (@<=@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary equal operation.
  -- @if(x == y) {}@
  infixl 4 @==@
  (@==@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary not equal operation.
  -- @if(x != y) {}@
  infixl 4 @!=@
  (@!=@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary logic and operation.
  -- @if(x && y) {}@
  infixl 3 @&&@
  (@&&@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Binary logic or operation.
  -- @if(x || y) {}@
  infixl 2 @||@
  (@||@)
    :: (Console c)
    => expr (KtValue c)  -- ^ Left argument of the operation.
    -> expr (KtValue c)  -- ^ Right argument of the operation.
    -> expr (KtValue c)  -- ^ Wrapped result of the operation.

  -- | Int constant.
  ktInt
    :: (Console c)
    => Int               -- ^ Target value.
    -> expr (KtValue c)  -- ^ Wrapped value.

  -- | Double constant.
  ktDouble
    :: (Console c)
    => Double            -- ^ Target value
    -> expr (KtValue c)  -- ^ Wrapped value.

  -- | String constant.
  ktString
    :: (Console c)
    => String            -- ^ Target value.
    -> expr (KtValue c)  -- ^ Wrapped value.

  -- | Bool constant.
  ktBool
    :: (Console c)
    => Bool              -- ^ Target value.
    -> expr (KtValue c)  -- ^ Wrapped value.

  -- | Unit constant.
  ktUnit
    :: (Console c)
    => ()                -- ^ Unit value.
    -> expr (KtValue c)  -- ^ Wrapped value.

-- | Type class for handling IO operations.
class Monad c => Console c where
  -- | Print given string.
  consolePrint :: String -> c ()
  
  -- | Print given string with appended @'\n'@.
  consolePrintln :: String -> c ()
  
  -- | Return String under monad.
  consoleReadLine :: c String

-- | List of wrapped functions type.
type KtDeclarations expr c = [expr (KtFunData c)]

-- | Data to handle function and variable scope of evaluation.
data KtScope c = KtScope
  { -- | All defined functions.
    sFun :: Map KtFunKey (KtFun c)
    
    -- | List of maps, every of which is variable area
    -- connected with close code block.
    -- Variable should be looked for from the first area to the last,
    -- names could be repeated in different areas, not in one.
  , sVariable :: [Map String (KtVariableInfo c)]
  }

-- | Type to handle variable info.
type KtVariableInfo c =
  ( Bool        -- ^ Set if variable is constant.
  , HiddenIO c  -- ^ Value of the variable.
  )

-- | Type of Kotlin file.
-- It returns nothing.
type KtFile c = c ()

-- | Type of the kotlin function
type KtFun c
   = KtScope c     -- ^ Scope of functions and variables.
  -> [HiddenIO c]  -- ^ List of arguments.
  -> HiddenIO c    -- ^ Some value that will be returned by call this function.

-- | Type to handle function info.
type KtFunData c =
  ( KtFunKey  -- ^ Unique by name and accepted types key.
  , KtFun c   -- ^ Real function.
  )

-- | Type of function key.
-- It unique by name and accepted types for every function.
type KtFunKey =
  ( Name         -- ^ Name of the function.
  , [KtAnyType]  -- ^ List of accepted types with saving order.
  )

-- | Type alias for Names of functions and variables.
type Name = String

-- | Type of function argument.
type KtFunArg =
  ( Name       -- | Name of the argument.
  , KtAnyType  -- | Type of the argument.
  )

-- | Type to handle values under scope.
type KtValue c = KtScope c -> HiddenIO c

-- | Type of the Kotlin command.
data KtCommand c where
  -- | Type of return.
  KtCmdReturn
    :: KtValue c    -- ^ Value that should be returned by the function.
    -> KtCommand c  -- ^ Constructed type

  -- | Type of command that could change scope.
  KtCmdStep
    :: (KtScope c -> c (KtScope c))  -- ^ Scope updater.
    -> KtCommand c                   -- ^ Constructed type.

  -- | Type of for
  KtCmdFor
    :: Name                         -- ^ Name of the iterator.
    -> [KtCommand c]                -- ^ Body - list of commands.
    -> (KtScope c -> c (Int, Int))  -- ^ Bound getter.
    -> KtCommand c                  -- ^ Constructed type.

  -- | Type of if
  KtCmdIf
    :: -- ^ Branches - list of pairs of scoped bool to command list.
       [(KtScope c -> c Bool, [KtCommand c])]
    -> KtCommand c  -- ^ Constructed type.

-- | Hide, but handle information about type of given monad.
-- Type for flexible work with monads of different types.
data HiddenIO c where
  HiddenIO
    :: (Typeable a)
    => KtType a    -- ^ Type of the hidden monad
    -> c a         -- ^ Target monad @c@ of type @a@
    -> HiddenIO c  -- ^ Constructed type.

-- | Data for mapping Kotlin to Haskell types.
data KtType t where
  KtIntType    :: KtType Int
  KtDoubleType :: KtType Double
  KtStringType :: KtType String
  KtUnitType   :: KtType ()
  KtBoolType   :: KtType Bool

-- | Data to hide type specification of @KtType@.
data KtAnyType where
  KtAnyType :: (Typeable t) => KtType t -> KtAnyType
