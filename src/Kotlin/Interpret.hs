{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpret
  ( Interpret(..)
  ) where

import Control.Monad  (liftM2)
import Data.Bifunctor (first)
import Data.Functor   ((<&>))
import Data.List      (intercalate)
import Data.Map       (Map, insert, empty, fromList, (!?))
import Data.Maybe     (catMaybes)
import Data.Typeable  (Typeable, (:~:)(..), eqT)
import GHC.Float      (int2Double)

import Kotlin.Dsl

-- | Type for interpreting some code.
newtype Interpret a = Interpret { interpret :: a }

-- | Specification how Kotlin should be interpreted.
instance Kotlin Interpret where
  -- | Put all functions to the scope and run @main@ function.
  -- Could fail if:
  --   * In given declarations presented functions with the same keys.
  --   * @main@ function not defined.
  ktFile :: (Console c) => KtDeclarations Interpret c -> Interpret (KtFile c)
  ktFile declarations = Interpret $ do
    let scope = KtScope
          { sFun      = toMap (interpret <$> declarations) ktIO
          , sVariable = []
          }
    case interpret (ktCallFun "main" []) scope of
      HiddenIO KtUnitType ioMain -> ioMain
      _ -> interpretError "`main(): Unit` function not defined!"
    where
      -- | Collect functions to the map.
      -- Fail if:
      --   * In given list presented functions with the same keys.
      toMap :: [KtFunData c] -> Map KtFunKey (KtFun c) -> Map KtFunKey (KtFun c)
      toMap []                m = m
      toMap ((key, fun):funs) m =
        case m !? key of
          Nothing -> toMap funs $ insert key fun m
          Just _  -> interpretError $
            "Multydefinition of function `" ++ uncurry getFunStr key ++ "`"

  -- | Construct kay and function.
  ktFun
    :: forall c. (Console c)
    => Name
    -> [KtFunArg]
    -> KtAnyType
    -> [Interpret (KtCommand c)]
    -> Interpret (KtFunData c)
  ktFun name argsInfo (KtAnyType rType) cmds =
    Interpret ((name, snd <$> argsInfo), ktFun)
    where
      -- | Construct Functions.
      -- Should be called with checked arguments.
      ktFun :: (Console c) => KtFun c
      ktFun initScope args = HiddenIO rType $ do
        argsValue <- evalArgs args  -- Support non-lazy behavior
        let scope = putArgs argsInfo argsValue initScope
        foldCommands scope rType $ interpret <$> cmds

      -- | Evaluate value under @HiddenIO@ and put it back again.
      evalArgs :: (Console c) => [HiddenIO c] -> c [HiddenIO c]
      evalArgs [] = return []
      evalArgs ((HiddenIO aType ioV):args) = do
        v <- ioV
        let hv = HiddenIO aType $ return v
        (hv :) <$> evalArgs args

      -- | Collect given arguments and its values to scope.
      -- @length argsInfo@ should be equal to @length argsValues@
      putArgs :: (Console c) => [KtFunArg] -> [HiddenIO c] -> KtScope c -> KtScope c
      putArgs argsInfo argsValues initScope
        | length argsInfo == length argsValues =
            go argsInfo argsValues
              -- We should clear variable area for eval function.
              $ initScope { sVariable = empty : [] }
        | otherwise =
            logicError $ withDiff  -- Should be checked on call, before interpret.
              ( "Incorrect count of args to call function "
                   ++ "`" ++ getFunStr name (snd <$> argsInfo) ++ "`."
              )
              ( show $ length argsInfo
              , show $ length argsValues
              )
        where
          -- _Note: pattern matching not full, should be called on lists with equal length._
          go :: (Console c) => [KtFunArg] -> [HiddenIO c] -> KtScope c -> KtScope c
          go [] [] scope = scope
          go ((aName, KtAnyType aType):is) (hv@(HiddenIO vType _):vs) scope
            | typeId aType == typeId vType =
                go is vs $ putVariable True aName hv scope
            | otherwise =
                logicError $ withDiff  -- Should be checked on call, before interpret
                  ( "Incorrect type of agument `" ++ aName ++ "` at function "
                      ++ "`" ++ getFunStr name (snd <$> argsInfo) ++ "`"
                  )
                  ( show aType
                  , show vType
                  )

  -- | Construct init variable command.
  -- Could fail if:
  --   * Variable with given name is already defined.
  --   * Given type is different with type of given value.
  ktInitVariable
    :: (Console c)
    => Bool
    -> Name
    -> KtAnyType
    -> Interpret (KtValue c)
    -> Interpret (KtCommand c)
  ktInitVariable isConstant name (KtAnyType aType) iValue =
    Interpret . KtCmdStep $ \scope -> case scope of
      KtScope { sVariable = []     } -> logicError "Scope havn't got varable area"
      KtScope { sVariable = vars:_ } ->
        case vars !? name of
          Nothing -> putVariableChecked isConstant name aType iValue scope
          Just _  -> interpretError ("Variable `" ++ name ++ "` is alrady defined")

  -- | Construct reassign variable command.
  -- Could fail if:
  --   * Variable with given name isn't defined.
  --   * Variable with given name is constant.
  --   * Type of variable is different with type of given value.
  ktSetVariable :: (Console c) => Name -> Interpret (KtValue c) -> Interpret (KtCommand c)
  ktSetVariable name iValue = Interpret . KtCmdStep $ \scope -> do
    case findVariable scope name of
      Nothing        -> interpretError $ "Variable `" ++ name ++ "` isn't defined."
      Just (True, _) -> interpretError $ "Variable `" ++ name ++ "` is immutable."
      Just (False, HiddenIO aType _) ->
        putVariableChecked False name aType iValue scope

  -- | Construct return command.
  ktReturn :: Interpret (KtValue c) -> Interpret (KtCommand c)
  ktReturn = Interpret . KtCmdReturn . interpret

  -- | Construct evaluate value command.
  ktValueCommand :: (Console c) => Interpret (KtValue c) -> Interpret (KtCommand c)
  ktValueCommand iv = Interpret . KtCmdStep $ \scope ->
    case interpret iv scope of
        HiddenIO _ ioa -> ioa >> return scope

  -- | Call function and return result of it execution.
  --
  -- Correctness of arguments' types on call supported by
  -- contract that by key in scope could be only function
  -- with the same signature.
  --
  -- Could fail if:
  --   * No function with given name and arguments' types.
  ktCallFun :: (Console c) => Name -> [Interpret (KtValue c)] -> Interpret (KtValue c)
  ktCallFun name iArgs = Interpret $ \scope ->
    let (aTypes, hValues) =
          unzip $ iArgs
            <&> flip interpret scope
            <&> \hv@(HiddenIO aType iov) -> (KtAnyType aType, hv)
     in case sFun scope !? (name, aTypes) of
          Just fun -> fun scope hValues
          Nothing  ->
            interpretError $
              "No funnction `" ++ getFunStr name aTypes ++ "` to call"

  -- | Find variable in the scope and return its value.
  -- Could fail if:
  --   * No variable with given name.
  ktReadVariable :: (Console c) => Name -> Interpret (KtValue c)
  ktReadVariable name = Interpret $ \scope ->
    case findVariable scope name of
      Nothing      -> interpretError $ "Variable `" ++ name ++ "` isn't defined."
      Just (_, hv) -> hv

  -- | Construct for command
  -- Could fail if:
  --   * Range bounds have incorrect type.
  ktFor
      :: (Console c)
      => Name
      -> Interpret (KtValue c)
      -> Interpret (KtValue c)
      -> [Interpret (KtCommand c)]
      -> Interpret (KtCommand c)
  ktFor name iFrom iTo iCmds =
    Interpret $ KtCmdFor name (interpret <$> iCmds) $ \scope ->
      case (interpret iFrom scope, interpret iTo scope) of
        (HiddenIO KtIntType iFrom, HiddenIO KtIntType iTo) -> do
          from <- iFrom
          to   <- iTo
          return (from, to)
        (HiddenIO lbType _, HiddenIO rbType _) ->
          interpretError $ withDiff
            "`for` range should has boundaries of type Int"
            ( "(Int, Int)"
            , "(" ++ show lbType ++ ", " ++ show rbType ++ ")"
            )

  -- | Construct if command
  -- Could fail if:
  --   * Some of conditions has incorrect type.
  ktIf
    :: (Console c)
    => [(Interpret (KtValue c), [Interpret (KtCommand c)])]
    -> [Interpret (KtCommand c)]
    -> Interpret (KtCommand c)
  ktIf branches elseBranch = Interpret . KtCmdIf $
    map unwrapBranch branches ++ [(\_ -> return True, interpret <$> elseBranch)]
    where
      unwrapBranch
        :: (Console c)
        => (Interpret (KtValue c), [Interpret (KtCommand c)])
        -> (KtScope c -> c Bool, [KtCommand c])
      unwrapBranch (iCondition, iCmds) =
        flip (,) (interpret <$> iCmds) $ \scope ->
          case interpret iCondition scope of
            HiddenIO KtBoolType ioCondition -> ioCondition
            HiddenIO aType _ -> interpretError $ withDiff
              "Condition should have type Bool"
              ( "Bool"
              , show aType
              )

  -- | See: 'unoOpPredator'
  ktNegate :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNegate = interpretUnoOp $ unoOpPredator
    { uOnInt    = UnoPutOp KtIntType    negate
    , uOnDouble = UnoPutOp KtDoubleType negate
    }

  -- | See: 'unoOpPredator'
  ktNot :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNot = interpretUnoOp $ unoOpPredator { uOnBool = UnoPutOp KtBoolType not }

  -- | See: 'binOpPredator'
  (@*@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@*@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    (*)
    , bOnDouble = BinPutOp KtDoubleType (*)
    }

  -- | See: 'binOpPredator'
  (@/@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@/@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    div
    , bOnDouble = BinPutOp KtDoubleType (/)
    }

  -- | See: 'binOpPredator'
  (@+@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@+@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    (+)
    , bOnDouble = BinPutOp KtDoubleType (+)
    , bOnString = BinPutOp KtStringType (++)
    }

  -- | See: 'binOpPredator'
  (@-@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@-@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    (-)
    , bOnDouble = BinPutOp KtDoubleType (-)
    }

  -- | See: 'binOpPredator'
  (@>@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@>@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (>)
    , bOnDouble = BinPutOp KtBoolType (>)
    , bOnString = BinPutOp KtBoolType (>)
    , bOnBool   = BinPutOp KtBoolType (>)
    }

  -- | See: 'binOpPredator'
  (@>=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@>=@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (>=)
    , bOnDouble = BinPutOp KtBoolType (>=)
    , bOnString = BinPutOp KtBoolType (>=)
    , bOnBool   = BinPutOp KtBoolType (>=)
    }

  -- | See: 'binOpPredator'
  (@<@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@<@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (<)
    , bOnDouble = BinPutOp KtBoolType (<)
    , bOnString = BinPutOp KtBoolType (<)
    , bOnBool   = BinPutOp KtBoolType (<)
    }

  -- | See: 'binOpPredator'
  (@<=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@<=@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (<=)
    , bOnDouble = BinPutOp KtBoolType (<=)
    , bOnString = BinPutOp KtBoolType (<=)
    , bOnBool   = BinPutOp KtBoolType (<=)
    }

  -- | See: 'binOpPredator'
  (@==@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@==@) = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = BinPutOp KtBoolType (==)
    , bOnDouble = BinPutOp KtBoolType (==)
    , bOnBool   = BinPutOp KtBoolType (==)
    , bOnString = BinPutOp KtBoolType (==)
    }

  -- | See: 'binOpPredator'
  (@!=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@!=@) = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = BinPutOp KtBoolType (/=)
    , bOnDouble = BinPutOp KtBoolType (/=)
    , bOnBool   = BinPutOp KtBoolType (/=)
    , bOnString = BinPutOp KtBoolType (/=)
    }

  -- | See: 'binOpPredator'
  (@&&@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@&&@) = interpretBinOp $ binOpPredator { bOnBool = BinPutOp KtBoolType (&&) }

  -- | See: 'binOpPredator'
  (@||@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@||@) = interpretBinOp $ binOpPredator { bOnBool = BinPutOp KtBoolType (||) }

  ktInt :: (Console c) => Int -> Interpret (KtValue c)
  ktInt = interpretConstant KtIntType

  ktDouble :: (Console c) => Double -> Interpret (KtValue c)
  ktDouble = interpretConstant KtDoubleType

  ktString :: (Console c) => String -> Interpret (KtValue c)
  ktString = interpretConstant KtStringType

  ktBool :: (Console c) => Bool -> Interpret (KtValue c)
  ktBool = interpretConstant KtBoolType

  ktUnit :: (Console c) => () -> Interpret (KtValue c)
  ktUnit = interpretConstant KtUnitType

-- | Define IO interface for Kotlin program.
ktIO :: (Console c) => Map KtFunKey (KtFun c)
ktIO = fromList
  [ ("print",   [KtAnyType KtIntType])    <-> printFun consolePrint
  , ("print",   [KtAnyType KtDoubleType]) <-> printFun consolePrint
  , ("print",   [KtAnyType KtStringType]) <-> printFun consolePrint
  , ("print",   [KtAnyType KtUnitType])   <-> printFun consolePrint
  , ("print",   [KtAnyType KtBoolType])   <-> printFun consolePrint
  , ("println", [KtAnyType KtIntType])    <-> printFun consolePrintln
  , ("println", [KtAnyType KtDoubleType]) <-> printFun consolePrintln
  , ("println", [KtAnyType KtStringType]) <-> printFun consolePrintln
  , ("println", [KtAnyType KtUnitType])   <-> printFun consolePrintln
  , ("println", [KtAnyType KtBoolType])   <-> printFun consolePrintln
  , ("readLine().toInt", []) <-> \_ [] ->
      HiddenIO KtIntType $ read @Int <$> consoleReadLine
  , ("readLine().toDouble", []) <-> \_ [] ->
      HiddenIO KtDoubleType $ read @Double <$> consoleReadLine
  , ("readLine", []) <-> \_ [] ->
      HiddenIO KtStringType consoleReadLine
  ]
  where
    -- | Create function that print with given printer given value.
    printFun
      :: (Console c) => (String -> c ()) -> KtScope c -> [HiddenIO c] -> HiddenIO c
    printFun sout = \_ [HiddenIO aType iov] ->
      HiddenIO KtUnitType $ iov >>= \v ->
        case aType of
          KtIntType    -> sout $ show v
          KtDoubleType -> sout $ show v
          KtStringType -> sout v
          KtUnitType   -> sout "kotlin.Unit"
          KtBoolType   -> sout $ if v then "true" else "false"

    infix 1 <->
    (<->) :: a -> b -> (a, b)
    (<->) = (,)

-- | Create new variable area on the top.
newVarArea :: (Console c) => KtScope c -> KtScope c
newVarArea scope = scope { sVariable = empty : sVariable scope }

-- | Delete top variable area from the scope.
popVarArea :: (Console c) => KtScope c -> KtScope c
popVarArea scope@(KtScope { sVariable = [] }) =
  logicError "No variable area to remove."
popVarArea scope@(KtScope { sVariable = _:vars }) =
  scope { sVariable = vars }

-- | Put variable in the top area.
putVariable :: (Console c) => Bool -> Name -> HiddenIO c -> KtScope c -> KtScope c
putVariable isConstant name hv scope =
  case sVariable scope of
    [] -> logicError "Scope havn't got varable area"
    vars:varsTail ->
      scope { sVariable = (insert name (isConstant, hv) vars) : varsTail }

-- | Check that type of given value the same as given type
-- and then put it if all are okay.
putVariableChecked
  :: (Console c)
  => Bool
  -> Name
  -> KtType t
  -> Interpret (KtValue c)
  -> KtScope c
  -> c (KtScope c)
putVariableChecked isConstant name aType iValue scope =
  case interpret iValue scope of
    HiddenIO vType iov
      | typeId vType == typeId aType -> do
          v <- iov  -- Support non-lazy behavior
          let hv = HiddenIO vType $ return v
          return $ putVariable isConstant name hv scope
      | otherwise -> interpretError $ withDiff
          "Initial value has incorrect type"
          ( show aType
          , show vType
          )

-- | Find variable with given name.
-- If in different scopes variables with the same name are presented,
-- will return information about the nearest.
findVariable :: (Console c) => KtScope c -> Name -> Maybe (KtVariableInfo c)
findVariable KtScope { sVariable = varsArea } name = processAreas varsArea
  where
    processAreas :: [Map String (KtVariableInfo c)] -> Maybe (KtVariableInfo c)
    processAreas []              = Nothing
    processAreas (vars:varsTail) =
      case vars !? name of
        Just vi -> Just vi
        Nothing -> processAreas varsTail

-- | Interpret function body.
foldCommands
  :: (Console c, Typeable a)
  => (KtScope c)    -- ^ Initial scope.
  -> KtType a       -- ^ Function expected return type.
  -> [KtCommand c]  -- ^ Function body.
  -> c a            -- ^ Value from the first interpreted return if it has correct type.
foldCommands scope (aType :: KtType a) cmds = do
  mbResult <- snd <$> foldCommands' scope cmds
  case mbResult of
    Just r  -> return r
    Nothing -> case aType of
      KtUnitType -> return ()
      _          -> interpretError "Missing return"
  where
    -- | Interpret command list and return optional result.
    foldCommands' :: (Console c) => KtScope c -> [KtCommand c] -> c (KtScope c, Maybe a)
    foldCommands' scope []         = return (scope, Nothing)
    foldCommands' scope (cmd:cmds) =
      case cmd of
        KtCmdStep ioS -> do
          s <- ioS scope
          foldCommands' s cmds
        KtCmdReturn value ->
          case value scope of
            HiddenIO (rType :: KtType r) ioR ->
              case eqT @a @r of
                Just Refl -> flip fmap ioR $ \r -> (scope, Just r)
                Nothing   -> interpretError $ withDiff
                  "foldCommands' run without checking return types"
                  ( show aType
                  , show rType
                  )
        KtCmdFor iName forCmds getRange -> do
          (from, to) <- getRange scope
          foldFor iName scope forCmds from to >>= \case
            (s, Nothing) -> foldCommands' s cmds
            (s, r)       -> return (s, r)
        KtCmdIf branches
          | length branches < 2 -> logicError $ withDiff
              "`if` command has less then 2 branches"
              ( ">= 2"
              , show $ length branches
              )
          | otherwise -> foldIf scope branches >>= \case
            (s, Nothing) -> foldCommands' s cmds
            (s, r)       -> return (s, r)

    -- | Interpret for command.
    foldFor
      :: (Console c)
      => Name -> KtScope c -> [KtCommand c] -> Int -> Int -> c (KtScope c, Maybe a)
    foldFor iName scope cmds from to
      | from > to = return (scope, Nothing)
      | otherwise = do
        let forS = putVariable False iName (HiddenIO KtIntType $ return from)
                     $ newVarArea scope
        foldCommands' forS cmds >>= \case
          (s, Nothing) -> foldFor iName (popVarArea s) cmds (succ from) to
          (s, r)       -> return (popVarArea s, r)

    -- | Interpret if command.
    foldIf
      :: (Console c)
      => KtScope c -> [(KtScope c -> c Bool, [KtCommand c])] -> c (KtScope c, Maybe a)
    foldIf scope [] = return (scope, Nothing)
    foldIf scope ((condition, cmds):branches) =
      condition scope >>= \case
        False -> foldIf scope branches
        True  -> first popVarArea <$> foldCommands' (newVarArea scope) cmds

-- | Create interpreter of constant value.
interpretConstant :: (Console c, Typeable a) => KtType a -> a -> Interpret (KtValue c)
interpretConstant aType a = Interpret $ \_ -> HiddenIO aType $ return a

-- | Helper data for defined unary operation.
data UnoOperation v r where
  -- | Operation not defined.
  UnoNotDefined :: UnoOperation v r

  -- | Operation with return type.
  UnoPutOp :: KtType r -> (v -> r) -> UnoOperation v r

-- | Helper data for specifying behavior of unary operation.
data UnoOpPredator i d b = UnoOpPredator
  { uOnInt    :: UnoOperation Int    i  -- ^ Operation for (Int) -> i
  , uOnDouble :: UnoOperation Double d  -- ^ Operation for (Double) -> d
  , uOnBool   :: UnoOperation Bool   b  -- ^ Operation for (Bool) -> b
  }

-- | Return UnoOpPredator without defined operations.
-- Useful with record update.
unoOpPredator :: UnoOpPredator Int Double Bool
unoOpPredator = UnoOpPredator
  { uOnInt      = UnoNotDefined
  , uOnDouble   = UnoNotDefined
  , uOnBool     = UnoNotDefined
  }

-- | Create interpret error for unary operation
-- with incorrect argument types.
unoOpError :: UnoOpPredator i d b -> KtType t -> a
unoOpError predator aType = interpretError $ withDiff
  "Invalid type of operation argument"
  ( getLegalTypesInfo predator
  , show aType
  )
  where
    getLegalTypesInfo :: UnoOpPredator i d b -> String
    getLegalTypesInfo predator = intercalate "," $ catMaybes
      [ mbStr KtIntType    $ uOnInt    predator
      , mbStr KtDoubleType $ uOnDouble predator
      , mbStr KtBoolType   $ uOnBool   predator
      ]

    mbStr :: KtType t -> UnoOperation v r -> Maybe String
    mbStr aType = \case
      UnoPutOp _ _ -> Just $ show aType
      _            -> Nothing

-- | Check that operation could be applied
-- on given value and return result of operation.
interpretUnoOp
  :: (Console c, Typeable i, Typeable d, Typeable b)
  => UnoOpPredator i d b    -- ^ Specification of operation.
  -> Interpret (KtValue c)  -- ^ Target value.
  -> Interpret (KtValue c)  -- ^ Result of operation.
interpretUnoOp predator iv =
  Interpret $ \scope ->
    case interpret iv scope of
      HiddenIO KtIntType    iov -> attack KtIntType    (uOnInt    predator) iov
      HiddenIO KtDoubleType iov -> attack KtDoubleType (uOnDouble predator) iov
      HiddenIO KtBoolType   iov -> attack KtBoolType   (uOnBool   predator) iov
      HiddenIO aType        _   -> unoOpError predator aType
  where
    attack :: (Console c, Typeable r) => KtType t -> UnoOperation a r -> c a -> HiddenIO c
    attack _ (UnoPutOp rType f) iov = HiddenIO rType $ f <$> iov
    attack aType UnoNotDefined  _   = unoOpError predator aType

-- | Helper data for defined binary operation.
data BinOperation vl vr r where
  -- | Operation not defined.
  BinNotDefined :: BinOperation vl vr r

  -- | Operation with return type.
  BinPutOp :: KtType r -> (vl -> vr -> r) -> BinOperation vl vr r

-- | Helper data for specifying behavior of binary operation.
data BinOpPredator i d b s u = BinOpPredator
  { bCanCast  :: Bool                          -- ^ Set if we could cast Int to Double.
  , bOnInt    :: BinOperation Int    Int    i  -- ^ Operation for (Int, Int) -> i
  , bOnDouble :: BinOperation Double Double d  -- ^ Operation for (Double, Double) -> d
  , bOnBool   :: BinOperation Bool   Bool   b  -- ^ Operation for (Bool, Bool) -> b
  , bOnString :: BinOperation String String s  -- ^ Operation for (String, String) -> s
  , bOnUnit   :: BinOperation ()     ()     u  -- ^ Operation for (Unit, Unit) -> u
  }

-- | Return BinOpPredator without defined operations.
-- Useful with record update.
binOpPredator :: BinOpPredator Int Double Bool String ()
binOpPredator = BinOpPredator
  { bCanCast  = True
  , bOnInt    = BinNotDefined
  , bOnDouble = BinNotDefined
  , bOnBool   = BinNotDefined
  , bOnString = BinNotDefined
  , bOnUnit   = BinNotDefined
  }

-- | Create interpret error for binary operation
-- with incorrect arguments types.
binOpError :: BinOpPredator i d b s u -> (KtType lt, KtType rt) -> a
binOpError predator (lType, rType) = interpretError $ withDiff
  "Invalid type of operation argument"
  ( getLegalTypesInfo predator
  , "(" ++ show lType ++ ", " ++ show rType ++ ")"
  )
  where
    getLegalTypesInfo :: BinOpPredator i d b s u -> String
    getLegalTypesInfo predator = intercalate "," . catMaybes $
      [ mbStr KtBoolType   $ bOnBool   predator
      , mbStr KtStringType $ bOnString predator
      , mbStr KtUnitType   $ bOnUnit   predator
      ] ++
      if bCanCast predator
      then
        [ Just "(Int or Double, Int or Double)" ]
      else
        [ mbStr KtIntType    $ bOnInt    predator
        , mbStr KtDoubleType $ bOnDouble predator
        ]
      where
        mbStr :: KtType t -> BinOperation lv rv r -> Maybe String
        mbStr aType = \case
          BinPutOp _ _ -> Just $ "(" ++ show aType ++ ", " ++ show aType ++ ")"
          _            -> Nothing

-- | Check that operation could be applied
-- on given arguments and return result of operation.
interpretBinOp
  :: (Console c, Typeable i, Typeable d, Typeable b, Typeable s, Typeable u)
  => BinOpPredator i d b s u  -- ^ Specification of operation.
  -> Interpret (KtValue c)    -- ^ Left argument.
  -> Interpret (KtValue c)    -- ^ Right argument.
  -> Interpret (KtValue c)    -- ^ Result of operation.
interpretBinOp predator il ir = Interpret $ \scope ->
  case (interpret il scope, interpret ir scope) of
    (HiddenIO typeL iol, HiddenIO typeR ior) ->
      case (typeL, typeR) of
        ii@(KtIntType,    KtIntType)    -> attack ii (bOnInt    predator) (iol, ior)
        dd@(KtDoubleType, KtDoubleType) -> attack dd (bOnDouble predator) (iol, ior)
        ss@(KtStringType, KtStringType) -> attack ss (bOnString predator) (iol, ior)
        bb@(KtBoolType,   KtBoolType)   -> attack bb (bOnBool   predator) (iol, ior)
        uu@(KtUnitType,   KtUnitType)   -> attack uu (bOnUnit   predator) (iol, ior)

        di@(KtDoubleType, KtIntType) ->
          if bCanCast predator
          then attack di (bOnDouble predator) (iol, int2Double <$> ior)
          else binOpError predator di
        id@(KtIntType, KtDoubleType) ->
          if bCanCast predator
          then attack id (bOnDouble predator) (int2Double <$> iol, ior)
          else binOpError predator id

        tt -> binOpError predator tt
  where
    attack
      :: (Console c, Typeable r)
      => (KtType lt, KtType rt) -> BinOperation a b r -> (c a, c b) -> HiddenIO c
    attack _ (BinPutOp rType f) (iol, ior) = HiddenIO rType $ liftM2 f iol ior
    attack tt BinNotDefined     _          = binOpError predator tt

-- | Send error message with "LOGIC ERROR" prefix.
logicError :: String -> a
logicError = error . ("LOGIC ERROR: " ++)

-- | Send error message with "INTERPRET ERROR" prefix.
interpretError :: String -> a
interpretError = errorWithoutStackTrace . ("INTERPRET ERROR: " ++)

-- | Construct diff message.
withDiff :: String -> (String, String) -> String
withDiff msg (expected, actual) =
    msg ++ "\n"
      ++ "Expected: " ++ expected ++ "\n"
      ++ "Actual:   " ++ actual   ++ "\n"

-- | Return string presentation of the given function key.
getFunStr :: Name -> [KtAnyType] -> String
getFunStr name args =
  name ++ "(" ++ (intercalate ", " $ show <$> args) ++ ")"

instance Show (KtType t) where
  show :: KtType t -> String
  show = \case
    KtIntType    -> "Int"
    KtDoubleType -> "Double"
    KtStringType -> "String"
    KtUnitType   -> "Unit"
    KtBoolType   -> "Bool"

-- | Cast @KtType@ to the id.
typeId :: KtType t -> Int
typeId = \case
    KtIntType    -> 0
    KtDoubleType -> 1
    KtStringType -> 2
    KtBoolType   -> 3
    KtUnitType   -> 4

-- | Cast @KtAnyType@ to the id.
-- Id will be the same as id of the hidden type.
aTypeId :: KtAnyType -> Int
aTypeId (KtAnyType t) = typeId t

instance Show KtAnyType where
  show :: KtAnyType -> String
  show (KtAnyType aType) = show aType

instance Eq KtAnyType where
  (==) :: KtAnyType -> KtAnyType -> Bool
  typeL == typeR = aTypeId typeL == aTypeId typeR

instance Ord KtAnyType where
  (<=) :: KtAnyType -> KtAnyType -> Bool
  typeL <= typeR = aTypeId typeL <= aTypeId typeR
