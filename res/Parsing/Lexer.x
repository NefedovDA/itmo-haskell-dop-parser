{
module Parsing.Lexer
  ( AlexPosn(..)
  , Token(..)

  , alexScanTokens

  , tPosition
  , tValue
  ) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z_]

tokens :-

    $white+                                     ;
    \/\*.*\*\/                                  ;

    true                                        { tok (\p s -> KeyTrue  p s) }
    false                                       { tok (\p s -> KeyFalse p s) }

    val                                         { tok (\p s -> KeyVal p s) }
    var                                         { tok (\p s -> KeyVar p s) }

    fun                                         { tok (\p s -> KeyFun p s) }

    return                                      { tok (\p s -> KeyReturn p s) }

    [$alpha][$alpha $digit]*                    { tok (\p s -> Name   p s) }

    [1-9][$digit]*|0                            { tok (\p s -> IntNum    p s) }
    [1-9][$digit]*\.[$digit]+|0\.[$digit]+      { tok (\p s -> DoubleNum p s) }

    \".*\"                                      { tok (\p s -> Str p s) }

    \(                                          { tok (\p s -> OCBracket p s) }
    \)                                          { tok (\p s -> CCBracket p s) }

    \{                                          { tok (\p s -> OBBracket p s) }
    \}                                          { tok (\p s -> CBBracket p s) }

    \+                                          { tok (\p s -> Plus  p s) }
    \-                                          { tok (\p s -> Minus p s) }
    \*                                          { tok (\p s -> Mull  p s) }
    \/                                          { tok (\p s -> Div   p s) }

    \=                                          { tok (\p s -> Equals p s) }

    \:                                          { tok (\p s -> Colon p s) }
    \;                                          { tok (\p s -> IEnd  p s) }

{

tok f p s = f p s

data Token
  = OCBracket AlexPosn String
  | CCBracket AlexPosn String

  | OBBracket AlexPosn String
  | CBBracket AlexPosn String

  | Plus      AlexPosn String
  | Minus     AlexPosn String
  | Mull      AlexPosn String
  | Div       AlexPosn String

  | Equals    AlexPosn String

  | IEnd      AlexPosn String
  | Colon     AlexPosn String

  | Name      AlexPosn String

  | IntNum    AlexPosn String
  | DoubleNum AlexPosn String

  | Str       AlexPosn String

  | KeyTrue   AlexPosn String
  | KeyFalse  AlexPosn String

  | KeyVal    AlexPosn String
  | KeyVar    AlexPosn String

  | KeyFun    AlexPosn String

  | KeyReturn AlexPosn String
  deriving (Show, Eq)

tPosition :: Token -> AlexPosn
tPosition (OCBracket p _) = p
tPosition (CCBracket p _) = p
tPosition (OBBracket p _) = p
tPosition (CBBracket p _) = p
tPosition (Plus      p _) = p
tPosition (Minus     p _) = p
tPosition (Mull      p _) = p
tPosition (Div       p _) = p
tPosition (Equals    p _) = p
tPosition (IEnd      p _) = p
tPosition (Colon     p _) = p
tPosition (Name      p _) = p
tPosition (IntNum    p _) = p
tPosition (DoubleNum p _) = p
tPosition (Str       p _) = p
tPosition (KeyTrue   p _) = p
tPosition (KeyFalse  p _) = p
tPosition (KeyVal    p _) = p
tPosition (KeyVar    p _) = p
tPosition (KeyFun    p _) = p
tPosition (KeyReturn p _) = p

tValue :: Token -> String
tValue (OCBracket _ v) = v
tValue (CCBracket _ v) = v
tValue (OBBracket _ v) = v
tValue (CBBracket _ v) = v
tValue (Plus      _ v) = v
tValue (Minus     _ v) = v
tValue (Mull      _ v) = v
tValue (Div       _ v) = v
tValue (Equals    _ v) = v
tValue (IEnd      _ v) = v
tValue (Colon     _ v) = v
tValue (Name      _ v) = v
tValue (IntNum    _ v) = v
tValue (DoubleNum _ v) = v
tValue (Str       _ v) = v
tValue (KeyTrue   _ v) = v
tValue (KeyFalse  _ v) = v
tValue (KeyVal    _ v) = v
tValue (KeyVar    _ v) = v
tValue (KeyFun    _ v) = v
tValue (KeyReturn _ v) = v

}