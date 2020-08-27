{
module Parsing.Parser
  ( happyParserExpression
  ) where

import qualified Parsing.Lexer         as T
import qualified Parsing.Expression    as E
import           Parsing.ParserHelper
}

%name           happyParserExpression
%tokentype      { T.Token }
%error          { parseError }
%monad          { Result } { thenE } { returnE }

%token
    TRUE        { T.Token T.KeyTrue  _ _ }
    FALSE       { T.Token T.KeyFalse _ _ }

    VAL         { T.Token T.KeyVal _ _ }
    VAR         { T.Token T.KeyVar _ _ }

    FUN         { T.Token T.KeyFun _ _ }

    RETURN      { T.Token T.KeyReturn _ _ }

    NAME        { T.Token T.Name _ $$ }

    INT_NUM     { T.Token T.IntNum    _ $$ }
    DBL_NUM     { T.Token T.DoubleNum _ $$ }

    STR         { T.Token T.Str _ $$ }

    '('         { T.Token T.OCBracket _ _ }
    ')'         { T.Token T.CCBracket _ _ }

    '{'         { T.Token T.OBBracket _ _ }
    '}'         { T.Token T.CBBracket _ _ }

    '+'         { T.Token T.Plus  _ _ }
    '-'         { T.Token T.Minus _ _ }
    '*'         { T.Token T.Mull  _ _ }
    '/'         { T.Token T.Div   _ _ }

    '='         { T.Token T.Equals   _ _ }

    ':'         { T.Token T.Colon _ _ }
    ';'         { T.Token T.IEnd  _ _ }
    ','         { T.Token T.Comma _ _ }

%%

File
  : FunctionList                          { E.File $1 }

FunctionList
  : Function FunctionList                 { $1 : $2 }
  |                                       { []      }

Function
  : FUN Name '(' ArgumentList ')' ':' Type '{' CommandList '}'   { E.Function $2 $4 $7 $9 }

ArgumentList
  : Argument ',' ArgumentList             { $1 : $3 }
  | Argument                              { [$1]    }
  |                                       { []      }

Argument
  : Name ':' Type                         { E.Argument $1 $3 }

CommandList
  : Command ';' CommandList               { $1 : $3 }
  |                                       { []      }

Command
  : Init                                  { E.InitCommand $1 }
  | Assignment                            { E.AssiCommand $1 }
  | Calculate                             { E.CalcCommand $1 }
  | Return                                { E.RetCommand  $1 }

Init
  : VAL Name ':' Type '=' Value           { E.Init $2 $4 $6 True  }
  | VAR Name ':' Type '=' Value           { E.Init $2 $4 $6 False }

Assignment
  : Name '=' Value                        { E.Assignment $1 $3 }

Return
  : RETURN Value                          { E.Return (Just $2) }
  | RETURN                                { E.Return Nothing   }

Value
  : Calculate                             { E.CalcValue $1 }
  | Str                                   { E.StrValue  $1 }
  | Boolean                               { E.BoolValue $1 }

Calculate
  : Calculate '+' Multiply                { E.PlusCalc  $1 $3 }
  | Calculate '-' Multiply                { E.MinusCalc $1 $3 }
  | Multiply                              { $1                }

Multiply
  : Multiply '*' Unary                    { E.MultCalc $1 $3 }
  | Multiply '/' Unary                    { E.DivCalc  $1 $3 }
  | Unary                                 { $1               }

Unary
  : '-' Unary                             { E.NegCalc $2 }
  | Indent                                { $1           }

Indent
  : '(' Calculate ')'                     { $2            }
  | CallFunction                          { E.CallCalc $1 }
  | Name                                  { E.NameCalc $1 }
  | Number                                { E.NumCalc  $1 }

CallFunction
  : Name '(' PassArgumentList ')'         { E.CallFunction $1 $3 }

PassArgumentList
  : Value ',' PassArgumentList            { $1 : $3 }
  | Value                                 { [$1]    }
  |                                       { []      }

Number
  : INT_NUM                               { E.IntNumber    $1 }
  | DBL_NUM                               { E.DoubleNumber $1 }

Str
  : STR                                   { E.Str $1 }

Boolean
  : TRUE                                  { True  }
  | FALSE                                 { False }

Name
  : NAME                                  { E.Name $1 }

Type
  : NAME                                  { E.Type $1 }
