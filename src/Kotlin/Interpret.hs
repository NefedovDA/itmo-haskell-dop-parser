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
import Data.Maybe (catMaybes)

newtype Interpret a = Interpret { interpret :: a }

instance Kotlin Interpret where
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

  ktSetVariable :: (Console c) => Name -> Interpret (KtValue c) -> Interpret (KtCommand c)
  ktSetVariable name iValue = Interpret . KtCmdStep $ \scope -> do
    case findVariable scope name of
      Nothing        -> interpretError $ "Variable `" ++ name ++ "` isn't defined."
      Just (True, _) -> interpretError $ "Variable `" ++ name ++ "` is immutable."
      Just (False, HiddenIO aType _) ->
        putVariableChecked False name aType iValue scope

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
    case findVariable scope name of
      Nothing      -> interpretError $ "Variable `" ++ name ++ "` isn't defined."
      Just (_, hv) -> hv

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
            HiddenIO aType _ -> interpretError $ withDiff
              "Condition should have type Bool"
              ( "Bool"
              , show aType
              )

  ktNegate :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNegate = interpretUnoOp $ unoOpPredator
    { uOnInt    = UnoPutOp KtIntType    negate
    , uOnDouble = UnoPutOp KtDoubleType negate
    }

  ktNot :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c)
  ktNot = interpretUnoOp $ unoOpPredator { uOnBool = UnoPutOp KtBoolType not }

  (@*@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@*@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    (*)
    , bOnDouble = BinPutOp KtDoubleType (*)
    }

  (@/@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@/@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    div
    , bOnDouble = BinPutOp KtDoubleType (/)
    }

  (@+@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@+@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    (+)
    , bOnDouble = BinPutOp KtDoubleType (+)
    , bOnString = BinPutOp KtStringType (++)
    }

  (@-@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@-@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtIntType    (-)
    , bOnDouble = BinPutOp KtDoubleType (-)
    }

  (@>@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@>@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (>)
    , bOnDouble = BinPutOp KtBoolType (>)
    , bOnString = BinPutOp KtBoolType (>)
    , bOnBool   = BinPutOp KtBoolType (>)
    }

  (@>=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@>=@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (>=)
    , bOnDouble = BinPutOp KtBoolType (>=)
    , bOnString = BinPutOp KtBoolType (>=)
    , bOnBool   = BinPutOp KtBoolType (>=)
    }

  (@<@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@<@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (<)
    , bOnDouble = BinPutOp KtBoolType (<)
    , bOnString = BinPutOp KtBoolType (<)
    , bOnBool   = BinPutOp KtBoolType (<)
    }

  (@<=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@<=@) = interpretBinOp $ binOpPredator
    { bOnInt    = BinPutOp KtBoolType (<=)
    , bOnDouble = BinPutOp KtBoolType (<=)
    , bOnString = BinPutOp KtBoolType (<=)
    , bOnBool   = BinPutOp KtBoolType (<=)
    }

  (@==@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@==@) = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = BinPutOp KtBoolType (==)
    , bOnDouble = BinPutOp KtBoolType (==)
    , bOnBool   = BinPutOp KtBoolType (==)
    , bOnString = BinPutOp KtBoolType (==)
    }

  (@!=@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@!=@) = interpretBinOp $ binOpPredator
    { bCanCast  = False
    , bOnInt    = BinPutOp KtBoolType (/=)
    , bOnDouble = BinPutOp KtBoolType (/=)
    , bOnBool   = BinPutOp KtBoolType (/=)
    , bOnString = BinPutOp KtBoolType (/=)
    }

  (@&&@) :: (Console c) => Interpret (KtValue c) -> Interpret (KtValue c) -> Interpret (KtValue c)
  (@&&@) = interpretBinOp $ binOpPredator { bOnBool = BinPutOp KtBoolType (&&) }

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

newVarArea :: (Console c) => KtScope c -> KtScope c
newVarArea scope = scope { sVariable = empty : sVariable scope }

popVarArea :: (Console c) => KtScope c -> KtScope c
popVarArea scope@(KtScope { sVariable = [] }) =
  logicError "No variable area to remove."
popVarArea scope@(KtScope { sVariable = _:vars }) =
  scope { sVariable = vars }

putVariable :: (Console c) => Bool -> Name -> HiddenIO c -> KtScope c -> KtScope c
putVariable isConstant name hv scope =
  case sVariable scope of
    [] -> logicError "Scope havn't got varable area"
    vars:varsTail ->
      scope { sVariable = (insert name (isConstant, hv) vars) : varsTail }

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

findVariable :: (Console c) => KtScope c -> Name -> Maybe (KtVariableInfo c)
findVariable KtScope { sVariable = varsArea } name = processAreas varsArea
  where
    processAreas :: [Map String (KtVariableInfo c)] -> Maybe (KtVariableInfo c)
    processAreas []              = Nothing
    processAreas (vars:varsTail) =
      case vars !? name of
        Just vi -> Just vi
        Nothing -> processAreas varsTail

foldCommands :: (Console c, Typeable a) => (KtScope c) -> KtType a -> [KtCommand c] -> c a
foldCommands scope (aType :: KtType a) cmds = do
  mbResult <- snd <$> foldCommands' scope cmds
  case mbResult of
    Just r  -> return r
    Nothing -> case aType of
      KtUnitType -> return ()
      _          -> interpretError "Missing return"
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

interpretConstant :: (Console c, Typeable a) => KtType a -> a -> Interpret (KtValue c)
interpretConstant aType a = Interpret $ \_ -> HiddenIO aType $ return a

data UnoOperation v r where
  UnoNotDefined :: UnoOperation v r
  UnoPutOp      :: KtType r -> (v -> r) -> UnoOperation v r

data UnoOpPredator i d b = UnoOpPredator
  { uOnInt      :: UnoOperation Int i
  , uOnDouble   :: UnoOperation Double d
  , uOnBool     :: UnoOperation Bool b
  }

unoOpPredator :: UnoOpPredator Int Double Bool
unoOpPredator = UnoOpPredator
  { uOnInt      = UnoNotDefined
  , uOnDouble   = UnoNotDefined
  , uOnBool     = UnoNotDefined
  } 

unoOpError :: UnoOpPredator i d b -> KtType t -> a
unoOpError predator aType = interpretError $ withDiff
  "Invalid type of operation argument"
  ( getLegalTypesInfo predator
  , show aType
  )
  where
    getLegalTypesInfo :: UnoOpPredator i d b -> String
    getLegalTypesInfo predator = intercalate "," $ catMaybes
      [ case uOnInt predator of
          UnoPutOp _ _ -> Just $ show KtIntType
          _            -> Nothing
      , case uOnDouble predator of
          UnoPutOp _ _ -> Just $ show KtDoubleType
          _            -> Nothing
      , case uOnBool predator of
          UnoPutOp _ _ -> Just $ show KtBoolType
          _            -> Nothing
      ]

interpretUnoOp
  :: (Console c, Typeable i, Typeable d, Typeable b)
  => UnoOpPredator i d b
  -> Interpret (KtValue c)
  -> Interpret (KtValue c)
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

data BinOperation vl vr r where
  BinNotDefined :: BinOperation vl vr r
  BinPutOp      :: KtType r -> (vl -> vr -> r) -> BinOperation vl vr r

data BinOpPredator i d b s u = BinOpPredator
  { bCanCast  :: Bool
  , bOnInt    :: BinOperation Int    Int    i
  , bOnDouble :: BinOperation Double Double d
  , bOnBool   :: BinOperation Bool   Bool   b
  , bOnString :: BinOperation String String s
  , bOnUnit   :: BinOperation ()     ()     u
  }

binOpPredator :: BinOpPredator Int Double Bool String ()
binOpPredator = BinOpPredator
  { bCanCast  = True
  , bOnInt    = BinNotDefined
  , bOnDouble = BinNotDefined
  , bOnBool   = BinNotDefined
  , bOnString = BinNotDefined
  , bOnUnit   = BinNotDefined
  }

binOpError :: BinOpPredator i d b s u -> (KtType lt, KtType rt) -> a
binOpError predator (lType, rType) = interpretError $ withDiff
  "Invalid type of operation argument"
  ( getLegalTypesInfo predator
  , "(" ++ show lType ++ ", " ++ show rType ++ ")"
  )
  where
    getLegalTypesInfo :: BinOpPredator i d b s u -> String
    getLegalTypesInfo predator = intercalate "," . catMaybes $
      [ case bOnBool predator of
          BinPutOp _ _ -> Just $ fromType KtBoolType
          _            -> Nothing
      , case bOnString predator of
          BinPutOp _ _ -> Just $ fromType KtStringType
          _            -> Nothing
      , case bOnUnit predator of
          BinPutOp _ _ -> Just $ fromType KtUnitType
          _            -> Nothing
      ] ++
      if bCanCast predator
      then
        [ Just "(Int or Double, Int or Double)" ]
      else
        [ case bOnInt predator of
          BinPutOp _ _ -> Just $ fromType KtIntType
          _            -> Nothing
        , case bOnDouble predator of
          BinPutOp _ _ -> Just $ fromType KtDoubleType
          _            -> Nothing
        ]
      where
        fromType :: KtType t -> String
        fromType aType = "(" ++ show aType ++ ", " ++ show aType ++ ")"

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
    attack :: (Console c, Typeable r) => (KtType lt, KtType rt) -> BinOperation a b r -> (c a, c b) -> HiddenIO c
    attack _ (BinPutOp rType f) (iol, ior) = HiddenIO rType $ liftM2 f iol ior
    attack tt BinNotDefined     _          = binOpError predator tt

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