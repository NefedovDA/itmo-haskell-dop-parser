{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Kotlin.Interpret
  ( Interpret(..)
  ) where

import Control.Monad  (liftM2)
import Data.Bifunctor (first)
import Data.List      (intercalate)
import Data.Map       (Map, insert, empty, fromList, (!?))
import Data.Monoid    (First(..))
import Data.Typeable  (Typeable, (:~:)(..), eqT)
import GHC.Float      (int2Double)

import Kotlin.Dsl
import Kotlin.Utils
import Data.Functor ((<&>))

newtype Interpret a = Interpret { interpret :: a }

instance Kotlin Interpret where
  ktFile :: forall c. (Console c) => KtDeclarations Interpret c -> Interpret (KtFile c)
  ktFile declarations = Interpret $ do
    let scope = KtScope
          { sFun      = toMap (interpret <$> declarations) ktIO
          , sVariable = []
          }
    case interpret (ktCallFun "main" []) scope of
      HiddenIO KtUnitType ioMain -> ioMain
      _ -> interpretError "`main(): Unit` function not defined!"
    where
      toMap :: [KtFunData c] -> Map KtFunKey (KtFun c) -> Map KtFunKey (KtFun c)
      toMap []                m = m
      toMap ((key, fun):funs) m =
        case m !? key of
          Nothing -> toMap funs $ insert key fun m
          Just _  -> interpretError $
            "Multydefinition of function `" ++ uncurry getFunStr key ++ "`"

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
      ktFun :: (Console c) => KtFun c
      ktFun initScope args = HiddenIO rType $ do
        argsValue <- evalArgs args  -- Support non-lazy behavior
        let scope = putArgs argsInfo argsValue initScope
        foldCommands scope rType $ interpret <$> cmds

      evalArgs :: (Console c) => [HiddenIO c] -> c [HiddenIO c]
      evalArgs [] = return []
      evalArgs ((HiddenIO aType ioV):args) = do
        v <- ioV
        let hv = HiddenIO aType $ return v
        (hv :) <$> evalArgs args

      putArgs :: (Console c) => [KtFunArg] -> [HiddenIO c] -> KtScope c -> KtScope c
      putArgs argsInfo argsValues initScope
        | length argsInfo == length argsValues =
            go argsInfo argsValues $ newVarArea initScope
        | otherwise =
            logicError $ withDiff  -- Should be checked on call, before interpret
              ( "Incorrect count of args to call function "
                   ++ "`" ++ getFunStr name (snd <$> argsInfo) ++ "`."
              )
              ( show $ length argsInfo
              , show $ length argsValues
              )
        where
          go :: (Console c) => [KtFunArg] -> [HiddenIO c] -> KtScope c -> KtScope c
          go [] [] scope = scope
          go ((aName, KtAnyType aType):is) (hv@(HiddenIO vType _):vs) scope
            | typeId aType == typeId vType =
                go is vs $ putValue aName hv scope
            | otherwise =
                logicError $ withDiff  -- Should be checked on call, before interpret
                  ( "Incorrect type of agument `" ++ aName ++ "` at function "
                      ++ "`" ++ getFunStr name (snd <$> argsInfo) ++ "`"
                  )
                  ( show aType
                  , show vType
                  )

  ktInitVariable
    :: (Console c)
    => Bool
    -> Name
    -> KtAnyType
    -> Interpret (KtValue c)
    -> Interpret (KtCommand c)
  ktInitVariable isConstant name aaType@(KtAnyType aType) iValue =
    Interpret . KtCmdStep $ \scope -> do
      let _ = checkOnTop scope name >> error "Variable name is alrady used"
      case interpret iValue scope of
        hv@(HiddenIO vType _)
          | typeId vType == typeId aType -> return $ putVariable isConstant name hv scope
          | otherwise                    -> error "Initial value has incorrect type"

  ktSetVariable :: (Console c) => Name -> Interpret (KtValue c) -> Interpret (KtCommand c)
  ktSetVariable name iValue = Interpret . KtCmdStep $ \scope -> do
    case findVariable scope name of
      Nothing -> error $ "No variable with name: " ++ name
      Just (True, _) -> error $ "Variable `" ++ name ++ "` is immutable"
      Just (False, HiddenIO aType _) ->
        case interpret iValue scope of
          hv@(HiddenIO vType _)
            | typeId vType == typeId aType -> return $ putVariable False name hv scope
            | otherwise        -> error "Value has incorrect type"

  ktReturn :: Interpret (KtValue c) -> Interpret (KtCommand c)
  ktReturn = Interpret . KtCmdReturn . interpret
  
  ktValueCommand :: (Console c) => Interpret (KtValue c) -> Interpret (KtCommand c)
  ktValueCommand iv = Interpret . KtCmdStep $ \scope ->
    case interpret iv scope of
        HiddenIO _ ioa -> ioa >> return scope


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

  ktReadVariable :: (Console c) => Name -> Interpret (KtValue c)
  ktReadVariable name = Interpret $ \scope ->
    snd $ fromJust
        ("No variable with name: " ++ name)
        (findVariable scope name)

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
        _ -> error "`for` range should has boundaries of type Int"

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
        flip to (interpret <$> iCmds) $ \scope ->
          case interpret iCondition scope of
            HiddenIO KtBoolType ioCondition -> ioCondition
            _ -> error "Condition should have type Bool"

  ktNegate :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNegate = interpretUnoOp $ unoOpPredator
    { uOnInt    = KtIntType    `to` negate
    , uOnDouble = KtDoubleType `to` negate
    }

  ktNot :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNot = interpretUnoOp $ unoOpPredator { uOnBool = KtBoolType `to` not }

  (@*@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@*@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (*)
    , bOnDouble = KtDoubleType `to` (*)
    }

  (@/@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@/@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` div
    , bOnDouble = KtDoubleType `to` (/)
    }

  (@+@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@+@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (+)
    , bOnDouble = KtDoubleType `to` (+)
    , bOnString = KtStringType `to` (++)
    }

  (@-@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@-@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtIntType    `to` (-)
    , bOnDouble = KtDoubleType `to` (-)
    }

  (@>@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@>@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (>)
    , bOnDouble = KtBoolType `to` (>)
    , bOnString = KtBoolType `to` (>)
    , bOnBool   = KtBoolType `to` (>)
    }

  (@>=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@>=@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (>=)
    , bOnDouble = KtBoolType `to` (>=)
    , bOnString = KtBoolType `to` (>=)
    , bOnBool   = KtBoolType `to` (>=)
    }

  (@<@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@<@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (<)
    , bOnDouble = KtBoolType `to` (<)
    , bOnString = KtBoolType `to` (<)
    , bOnBool   = KtBoolType `to` (<)
    }

  (@<=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@<=@) = interpretBinOp $ binOpPredator
    { bOnInt    = KtBoolType `to` (<=)
    , bOnDouble = KtBoolType `to` (<=)
    , bOnString = KtBoolType `to` (<=)
    , bOnBool   = KtBoolType `to` (<=)
    }

  (@==@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@==@) = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = KtBoolType `to` (==)
    , bOnDouble = KtBoolType `to` (==)
    , bOnBool   = KtBoolType `to` (==)
    , bOnString = KtBoolType `to` (==)
    }

  (@!=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@!=@) = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = KtBoolType `to` (/=)
    , bOnDouble = KtBoolType `to` (/=)
    , bOnBool   = KtBoolType `to` (/=)
    , bOnString = KtBoolType `to` (/=)
    }

  (@&&@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@&&@) = interpretBinOp $ binOpPredator { bOnBool = KtBoolType `to` (&&) }

  (@||@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@||@) = interpretBinOp $ binOpPredator { bOnBool = KtBoolType `to` (||) }

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

newVarArea :: (Console c) => KtScope c -> KtScope c
newVarArea scope = scope { sVariable = empty : sVariable scope }

popVarArea :: (Console c) => KtScope c -> KtScope c
popVarArea scope@(KtScope { sVariable = [] }) =
  error "LOGIC ERROR (Try remove from empty variable area)"
popVarArea scope@(KtScope { sVariable = _:vars }) =
  scope { sVariable = vars }

putVariable :: (Console c) => Bool -> Name -> HiddenIO c -> KtScope c -> KtScope c
putVariable isConstant name hv scope =
  case sVariable scope of
    [] -> error "Scope havn't got varable area"
    vars:varsTail ->
      scope { sVariable = (insert name (isConstant, hv) vars) : varsTail }

putValue :: (Console c) => Name -> HiddenIO c -> KtScope c -> KtScope c
putValue = putVariable True

findVariable :: (Console c) => KtScope c -> Name -> Maybe (KtVariableInfo c)
findVariable KtScope { sVariable = varsArea } name = processAreas varsArea
  where
    processAreas :: [Map String (KtVariableInfo c)] -> Maybe (KtVariableInfo c)
    processAreas []              = Nothing
    processAreas (vars:varsTail) =
      case vars !? name of
        Just vi -> Just vi
        Nothing -> processAreas varsTail

checkOnTop :: (Console c) => KtScope c -> Name -> Maybe (KtVariableInfo c)
checkOnTop KtScope { sVariable = [] }     name = error "Scope havn't got varable area"
checkOnTop KtScope { sVariable = vars:_ } name = vars !? name

ktIO :: (Console c) => Map KtFunKey (KtFun c)
ktIO = fromList
  [ ("print", [KtAnyType KtIntType])    `to` printFun1 consolePrint
  , ("print", [KtAnyType KtDoubleType]) `to` printFun1 consolePrint
  , ("print", [KtAnyType KtStringType]) `to` printFun1 consolePrint
  , ("print", [KtAnyType KtUnitType])   `to` printFun1 consolePrint
  , ("print", [KtAnyType KtBoolType])   `to` printFun1 consolePrint

  , ("println", [KtAnyType KtIntType])    `to` printFun1 consolePrintln
  , ("println", [KtAnyType KtDoubleType]) `to` printFun1 consolePrintln
  , ("println", [KtAnyType KtStringType]) `to` printFun1 consolePrintln
  , ("println", [KtAnyType KtUnitType])   `to` printFun1 consolePrintln
  , ("println", [KtAnyType KtBoolType])   `to` printFun1 consolePrintln

  , ("readLine().toInt", []) `to` \_ [] ->
      HiddenIO KtIntType $ read @Int <$> consoleReadLine
  , ("readLine().toDouble", []) `to` \_ [] ->
      HiddenIO KtDoubleType $ read @Double <$> consoleReadLine
  , ("readLine", []) `to` \_ [] ->
      HiddenIO KtStringType consoleReadLine
  ]
  where
    printFun1
      :: (Console c) => (String -> c ()) -> KtScope c -> [HiddenIO c] -> HiddenIO c
    printFun1 sout = \_ [HiddenIO aType iov] ->
      HiddenIO KtUnitType $ iov >>= \v ->
        case aType of
          KtIntType    -> sout $ show v
          KtDoubleType -> sout $ show v
          KtStringType -> sout v
          KtUnitType   -> sout "kotlin.Unit"
          KtBoolType   -> sout $ if v then "true" else "false"

interpretConstant :: (Console c, Typeable a) => KtType a -> a -> Interpret (KtValue c)
interpretConstant aType a = Interpret $ \_ -> HiddenIO aType $ return a

foldCommands :: (Console c, Typeable a) => (KtScope c) -> KtType a -> [KtCommand c] -> c a
foldCommands scope (aType :: KtType a) cmds = do
  mbResult <- snd <$> foldCommands' scope cmds
  case mbResult of
    Just r  -> return r
    Nothing -> case aType of
      KtUnitType -> return ()
      _          -> error "Missing return"
  where
    foldCommands' :: (Console c) => KtScope c -> [KtCommand c] -> c (KtScope c, Maybe a)
    foldCommands' scope []         = return (scope, Nothing)
    foldCommands' scope (cmd:cmds) =
      case cmd of
        KtCmdStep ioS -> do
          s <- ioS scope
          foldCommands' s cmds
        KtCmdReturn value ->
          case value scope of
            HiddenIO (_ :: KtType r) ioR ->
              case eqT @a @r of
                Just Refl -> flip fmap ioR $ \r -> (scope, Just r)
                Nothing   -> error "LOGIC ERROR (foldCommands' run without checking return types)"
        KtCmdFor iName forCmds getRange -> do
          (from, to) <- getRange scope
          foldFor iName scope forCmds from to >>= \case
            (s, Nothing) -> foldCommands' s cmds
            (s, r)       -> return (s, r)
        KtCmdIf branches
          | length branches < 2 -> error "LOGIC ERROR (if command has less then 2 branches)"
          | otherwise           -> foldIf scope branches >>= \case
            (s, Nothing) -> foldCommands' s cmds
            (s, r)       -> return (s, r)

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

    foldIf
      :: (Console c)
      => KtScope c -> [(KtScope c -> c Bool, [KtCommand c])] -> c (KtScope c, Maybe a)
    foldIf scope [] = return (scope, Nothing)
    foldIf scope ((condition, cmds):branches) =
      condition scope >>= \case
        False -> foldIf scope branches
        True  -> first popVarArea <$> foldCommands' (newVarArea scope) cmds

data UnoOpPredator i d b = UnoOpPredator
  { uOnInt    :: (KtType i, Int -> i)
  , uOnDouble :: (KtType d, Double -> d)
  , uOnBool   :: (KtType b, Bool -> b)
  }

unoOpPredator :: UnoOpPredator Int Double Bool
unoOpPredator = UnoOpPredator
  { uOnInt    = (KtIntType,    unoError)
  , uOnDouble = (KtDoubleType, unoError)
  , uOnBool   = (KtBoolType,   unoError)
  }
  where
    unoError :: a -> a
    unoError _ = error "Invalid type of operation argument"

interpretUnoOp
  :: (Console c, Typeable i, Typeable d, Typeable b)
  => UnoOpPredator i d b
  -> Interpret (KtValue c)
  -> Interpret (KtValue c)
interpretUnoOp predator iv =
  Interpret $ \scope ->
    case interpret iv scope of
      (HiddenIO KtIntType    iov) -> uOnInt    predator `attack` iov
      (HiddenIO KtDoubleType iov) -> uOnDouble predator `attack` iov
      (HiddenIO KtBoolType   iov) -> uOnBool   predator `attack` iov
      _                           -> error "Invalid type of operation argument"
  where
    attack :: (Console c, Typeable r) => (KtType r, a -> r) -> c a -> HiddenIO c
    attack (rType, f) iov = HiddenIO rType $ f <$> iov

data BinOpPredator i d b s u = BinOpPredator
  { bCanCast  :: Bool
  , bOnInt    :: (KtType i, Int -> Int -> i)
  , bOnDouble :: (KtType d, Double -> Double -> d)
  , bOnBool   :: (KtType b, Bool -> Bool -> b)
  , bOnString :: (KtType s, String -> String -> s)
  , bOnUnit   :: (KtType u, () -> () -> u)
  }

binOpPredator :: BinOpPredator Int Double Bool String ()
binOpPredator = BinOpPredator
  { bCanCast  = True
  , bOnInt    = (KtIntType,    binError)
  , bOnDouble = (KtDoubleType, binError)
  , bOnBool   = (KtBoolType,   binError)
  , bOnString = (KtStringType, binError)
  , bOnUnit   = (KtUnitType,   binError)
  }
  where
    binError :: a -> a -> a
    binError _ _ = error "Invalid types of operation arguments"

interpretBinOp
  :: (Console c, Typeable i, Typeable d, Typeable b, Typeable s, Typeable u)
  => BinOpPredator i d b s u
  -> Interpret (KtValue c)
  -> Interpret (KtValue c)
  -> Interpret (KtValue c)
interpretBinOp predator il ir = Interpret $ \scope ->
  case (interpret il scope, interpret ir scope) of
    (HiddenIO typeL iol, HiddenIO typeR ior) ->
      case (typeL, typeR) of
        (KtIntType,    KtIntType)    -> bOnInt    predator `attack` (iol, ior)
        (KtDoubleType, KtDoubleType) -> bOnDouble predator `attack` (iol, ior)
        (KtStringType, KtStringType) -> bOnString predator `attack` (iol, ior)
        (KtBoolType,   KtBoolType)   -> bOnBool   predator `attack` (iol, ior)
        (KtUnitType,   KtUnitType)   -> bOnUnit   predator `attack` (iol, ior)

        (KtDoubleType, KtIntType) ->
          if bCanCast predator
          then bOnDouble predator `attack` (iol, int2Double <$> ior)
          else binError
        (KtIntType, KtDoubleType) ->
          if bCanCast predator
          then bOnDouble predator `attack` (int2Double <$> iol, ior)
          else binError

        _ -> binError
  where
    attack :: (Console c, Typeable r) => (KtType r, a -> b -> r) -> (c a, c b) -> HiddenIO c
    attack (rType, f) (iol, ior) = HiddenIO rType $ liftM2 f iol ior

    binError :: a
    binError = error "Invalid types of operation arguments"

logicError :: String -> a
logicError = error . ("LOGIC ERROR: " ++)

interpretError :: String -> a
interpretError = error . ("INTERPRET ERROR: " ++)

withDiff :: String -> (String, String) -> String
withDiff msg (expected, actual) =
    msg ++ "\n"
      ++ "Expected: " ++ expected ++ "\n"
      ++ "Actual:   " ++ actual   ++ "\n"

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

typeId :: KtType t -> Int
typeId = \case
    KtIntType    -> 0
    KtDoubleType -> 1
    KtStringType -> 2
    KtBoolType   -> 3
    KtUnitType   -> 4

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