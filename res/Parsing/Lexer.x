{
module Parsing.Lexer
  ( AlexPosn(..)
  , Token(..)
  , TokenType(..)

  , alexScanTokens
  ) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z_]

tokens :-

    $white+                                     ;
    \/\*.*\*\/                                  ;

    true                                        { tok KeyTrue  }
    false                                       { tok KeyFalse }

    val                                         { tok KeyVal }
    var                                         { tok KeyVar }

    fun                                         { tok KeyFun }

    return                                      { tok KeyReturn }

    [$alpha][$alpha $digit]*                    { tok Name }

    [1-9][$digit]*|0                            { tok IntNum    }
    [1-9][$digit]*\.[$digit]+|0\.[$digit]+      { tok DoubleNum }

    \".*\"                                      { tok Str }

    \(                                          { tok OCBracket }
    \)                                          { tok CCBracket }

    \{                                          { tok OBBracket }
    \}                                          { tok CBBracket }

    \+                                          { tok Plus  }
    \-                                          { tok Minus }
    \*                                          { tok Mull  }
    \/                                          { tok Div   }

    \=                                          { tok Equals }

    \:                                          { tok Colon }
    \;                                          { tok IEnd  }

{

tok :: TokenType -> AlexPosn -> String -> Token
tok t p s = Token t p s

data TokenType
  = OCBracket
  | CCBracket
  | OBBracket
  | CBBracket
  | Plus
  | Minus
  | Mull
  | Div
  | Equals
  | IEnd
  | Colon
  | Name
  | IntNum
  | DoubleNum
  | Str
  | KeyTrue
  | KeyFalse
  | KeyVal
  | KeyVar
  | KeyFun
  | KeyReturn
  deriving (Show, Eq)

data Token = Token
  { tType     :: TokenType
  , tPosition :: AlexPosn
  , tValue    :: String
  } deriving (Show, Eq)

}