{
module Parsing.Parser
  ( happyParserExpression
  ) where

import           Parsing.KotlinPsi (KotlinPsi(..))
import qualified Parsing.KotlinPsi as KT
import qualified Parsing.Result    as R
import qualified Parsing.Token     as T
import qualified Parsing.Utils     as U
}

%name           happyParserExpression
%tokentype      { T.Token }
%error          { R.parseError }
%monad          { R.Result } { R.thenE } { R.returnE }

%token
    TRUE        { T.Token T.KeyTrue  _ _ }
    FALSE       { T.Token T.KeyFalse _ _ }

    VAL         { T.Token T.KeyVal _ _ }
    VAR         { T.Token T.KeyVar _ _ }

    FUN         { T.Token T.KeyFun _ _ }

    RETURN      { T.Token T.KeyReturn _ _ }

    FOR         { T.Token T.KeyFor _ _ }
    IN          { T.Token T.KeyIn  _ _ }

    IF          { T.Token T.KeyIf   _ _ }
    ELSE        { T.Token T.KeyElse _ _ }

    UNIT        { T.Token T.TypeUnit   _ _ }
    INT         { T.Token T.TypeInt    _ _ }
    DOUBLE      { T.Token T.TypeDouble _ _ }
    BOOL        { T.Token T.TypeBool   _ _ }
    STRING      { T.Token T.TypeString _ _ }

    NAME        { T.Token T.Name _ $$ }

    INT_NUM     { T.Token T.IntNum    _ $$ }
    DBL_NUM     { T.Token T.DoubleNum _ $$ }

    STR         { T.Token T.Str _ $$ }

    '+'         { T.Token T.Plus  _ _ }
    '-'         { T.Token T.Minus _ _ }
    '*'         { T.Token T.Mull  _ _ }
    '/'         { T.Token T.Div   _ _ }

    '&&'        { T.Token T.And _ _ }
    '||'        { T.Token T.Or  _ _ }
    '!'         { T.Token T.Not _ _ }

    '=='        { T.Token T.Eq     _ _ }
    '!='        { T.Token T.NotEq  _ _ }

    '>'         { T.Token T.Gt  _ _ }
    '>='        { T.Token T.Gte _ _ }
    '<'         { T.Token T.Lt  _ _ }
    '<='        { T.Token T.Lte _ _ }

    '('         { T.Token T.OCBracket _ _ }
    ')'         { T.Token T.CCBracket _ _ }

    '{'         { T.Token T.OBBracket _ _ }
    '}'         { T.Token T.CBBracket _ _ }

    '='         { T.Token T.Equals   _ _ }

    ':'         { T.Token T.Colon  _ _ }
    ';'         { T.Token T.IEnd   _ _ }
    ','         { T.Token T.Comma  _ _ }
    '..'        { T.Token T.DPoint _ _ }

%%

File
  : FunList                                       { KtPsiFile $1 }

FunList
  : Fun FunList                                   { $1 : $2 }
  |                                               { []      }

Fun
  : FUN NAME '(' ArgList ')' ':' Type '{' CommandList '}'    { KtPsiFun $2 $4 $7 $9 }

ArgList
  : Arg ',' ArgList                               { $1 : $3 }
  | Arg                                           { [$1]    }
  |                                               { []      }

Arg
  : NAME ':' Type                                 { ($1, $3) }

CommandList
  : Command CommandList                           { $1 : $2 }
  |                                               { []      }

Command
  : Return    ';'                                 { $1 }
  | JustValue ';'                                 { $1 }
  | InitVar   ';'                                 { $1 }
  | SetVar    ';'                                 { $1 }
  | For                                           { $1 }
  | If                                            { $1 }

Return
  : RETURN                                        { U.defaultReturn }
  | RETURN Value                                  { KtPsiReturn $2  }

JustValue
  : Value                                         { KtPsiValueCommand $1 }

InitVar
  : VAL NAME ':' Type '=' Value                   { KtPsiInitVariable True  $2 $4 $6 }
  | VAR NAME ':' Type '=' Value                   { KtPsiInitVariable False $2 $4 $6 }

SetVar
  : NAME '=' Value                                { KtPsiSetVariable $1 $3 }

For
  : FOR '(' NAME IN Value '..' Value ')' '{' CommandList '}'    { KtPsiFor $3 $5 $7 $10 }

If
  : IfBranch ELSE If                              { U.addBranch $1  $3 }
  | IfBranch ELSE '{' CommandList '}'             { KtPsiIf [$1] $4    }
  | IfBranch                                      { KtPsiIf [$1] []    }

IfBranch
  : IF '(' Value ')' '{' CommandList '}'          { ($3, $6) }

Value
  : Or                                            { $1 }

Or
  : Or '||' And                                   { $1 :||: $3 }
  | And                                           { $1         }

And
  : And '&&' Eq                                   { $1 :&&: $3 }
  | Eq                                            { $1         }

Eq
  : Eq '==' Comp                                  { $1 :==: $3 }
  | Eq '!=' Comp                                  { $1 :!=: $3 }
  | Comp                                          { $1         }

Comp
  : Comp '>'  Plus                                { $1 :>:  $3 }
  | Comp '<'  Plus                                { $1 :<:  $3 }
  | Comp '>=' Plus                                { $1 :>=: $3 }
  | Comp '<=' Plus                                { $1 :<=: $3 }
  | Plus                                          { $1         }

Plus
  : Plus '+' Mult                                 { $1 :+: $3 }
  | Plus '-' Mult                                 { $1 :-: $3 }
  | Mult                                          { $1        }

Mult
  : Mult '*' Unary                                { $1 :*: $3 }
  | Mult '/' Unary                                { $1 :/: $3 }
  | Unary                                         { $1        }

Unary
  : '!' Unary                                     { KtPsiNot    $2 }
  | '-' Unary                                     { KtPsiNegate $2 }
  | Target                                        { $1             }


Target
  : '(' Value ')'                                 { $2 }
  | CallFun                                       { $1 }
  | ReadVar                                       { $1 }
  | Double                                        { $1 }
  | Int                                           { $1 }
  | String                                        { $1 }
  | Bool                                          { $1 }
  | Unit                                          { $1 }

CallFun
  : NAME '(' ValueList ')'                        { KtPsiCallFun $1 $3 }

ValueList
  : Value ',' ValueList                           { $1 : $3 }
  | Value                                         { [$1]    }
  |                                               { []      }

ReadVar
  : NAME                                          { KtPsiReadVariable $1 }

Int
  : INT_NUM                                       {% U.checkedInt $1 }

Double
  : DBL_NUM                                       {% U.checkedDouble $1 }

String
  : STR                                           { U.updatedString $1 }

Bool
  : TRUE                                          { KtPsiBool True  }
  | FALSE                                         { KtPsiBool False }

Unit
  : UNIT                                          { KtPsiUnit () }

Type
  : UNIT                                          { KT.KtAnyType KT.KtUnitType   }
  | INT                                           { KT.KtAnyType KT.KtIntType    }
  | DOUBLE                                        { KT.KtAnyType KT.KtDoubleType }
  | BOOL                                          { KT.KtAnyType KT.KtBoolType   }
  | STRING                                        { KT.KtAnyType KT.KtStringType }
