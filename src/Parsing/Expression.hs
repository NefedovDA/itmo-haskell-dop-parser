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
  { ifBranches :: [Branch]
  , ifDefault  :: [Command]
  } deriving (Eq, Show)

data Branch = Branch
  { bCondition :: Condition
  , bBody      :: [Command]
  } deriving (Eq, Show)

addBranch :: Branch -> If -> If
addBranch branch baseIf = baseIf { ifBranches = branch : ifBranches baseIf }

data Value 
  = NumValue  Number
  | StrValue  Str
  | BoolValue Bool
  | NameValue Name
  | CallValue CallFunction
  deriving (Eq, Show)

data Condition
  = BoolCondition Bool
  | NameCondition Name
  | CallCondition CallFunction
  deriving (Eq, Show)

data Assignment = Assignment
  { aTarget :: Name
  , aValue  :: Value
  } deriving (Eq, Show)

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

  