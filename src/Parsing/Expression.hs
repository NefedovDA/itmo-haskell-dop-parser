module Parsing.Expression where

newtype File = File { unFile :: [Function] }
  deriving (Eq, Show)

data Function = Function
 { fName       :: Name
 , fArguments  :: [Argument]
 , fReturnType :: Type
 , fBody       :: [Command]
 } deriving (Eq, Show)

data Argument = Argument
  { aName :: Name
  , aType :: Type
  } deriving (Eq, Show)

newtype Name = Name { unName :: String }
  deriving (Eq, Show)
newtype Type = Type { unType :: String }
  deriving (Eq, Show)

data Command 
  = InitCommand Init
  | AssiCommand Assignment
  | CalcCommand Calculate
  | RetCommand  Return
  | ForCommand  For
  | IfCommand   If
  deriving (Eq, Show)

data Init = Init
   { iName    :: Name
   , iType    :: Type
   , iValue   :: Value
   , iIsConst :: Bool
   } deriving (Eq, Show)

data For = For
  { fIndexName :: Name
  , fFrom      :: Number
  , fTo        :: Number
  , forBody    :: [Command]
  } deriving (Eq, Show)

data If = If
  { ifBranches :: [(Condition, [Command])]
  , ifDefault  :: [Command]
  } deriving (Eq, Show)

addBranch :: (Condition, [Command]) -> If -> If
addBranch branch baseIf = baseIf { ifBranches = branch : ifBranches baseIf }

data Condition
  = BoolCondition Bool
  | NameCondition Name
  | NotCondition Condition
  | AndCondition Condition Condition
  | OrCondition  Condition Condition
  deriving (Eq, Show)

data Value 
  = CalcValue Calculate
  | StrValue  Str
  | BoolValue Bool
  | NameValue Name
  deriving (Eq, Show)

data Assignment = Assignment
  { aTarget :: Name
  , aValue  :: Value
  } deriving (Eq, Show)

data Calculate
  = CallCalc  CallFunction
  | NameCalc  Name
  | NumCalc   Number
  | NegCalc   Calculate
  | MultCalc  Calculate Calculate
  | DivCalc   Calculate Calculate
  | PlusCalc  Calculate Calculate
  | MinusCalc Calculate Calculate
  deriving (Eq, Show)

newtype Return = Return { unReturn :: Maybe Value }
  deriving (Eq, Show)

data Number
  = IntNumber    { nValue :: String }
  | DoubleNumber { nValue :: String }
  deriving (Eq, Show)

newtype Str = Str { unStr :: String }
  deriving (Eq, Show)

data CallFunction = CallFunction
  { cfName      :: Name
  , cfArguments :: [Value]
  } deriving (Eq, Show)

  