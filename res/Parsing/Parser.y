{
module Parsing.Parser
  ( happyParserExpression
  ) where

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
  : FunDecl                                       { KT.KtPsiFile $ U.unproxyFunDec $1 }

FunDecl
  : Fun0 FunDecl                                  {% U.putFun0Data $1 $2 }
  | Fun1 FunDecl                                  {% U.putFun1Data $1 $2 }
  | Fun2 FunDecl                                  {% U.putFun2Data $1 $2 }
  |                                               {  U.emptyProxyFunDec  }

Fun0
  : FUN NAME '(' ')' ':' Type '{' CommandList '}'    { KT.KtPsiFun0 $2 $6 $8 }

Fun1
  : FUN NAME '(' Arg ')' ':' Type '{' CommandList '}'    { KT.KtPsiFun1 $2 $4 $7 $9 }

Fun2
  : FUN NAME '(' Arg ',' Arg ')' ':' Type '{' CommandList '}'    { KT.KtPsiFun2 $2 $4 $6 $9 $11 }

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

Return
  : RETURN                                        { U.defaultReturn   }
  | RETURN Value                                  { KT.KtPsiReturn $2 }

JustValue
  : Value                                         { KT.KtPsiValueCommand $1 }

InitVar
  : VAL NAME ':' Type '=' Value                   { KT.KtPsiInitVariable True  $2 $4 $6 }
  | VAR NAME ':' Type '=' Value                   { KT.KtPsiInitVariable False $2 $4 $6 }

SetVar
  : NAME '=' Value                                { KT.KtPsiSetVariable $1 $3 }

Value
  : Or                                            { $1 }

Or
  : Or '||' And                                   { KT.KtPsiOr $1 $3 }
  | And                                           { $1               }

And
  : And '&&' Eq                                   { KT.KtPsiAnd $1 $3 }
  | Eq                                            { $1                }

Eq
  : Eq '==' Comp                                  { KT.KtPsiEq    $1 $3 }
  | Eq '!=' Comp                                  { KT.KtPsiNotEq $1 $3 }
  | Comp                                          { $1                  }

Comp
  : Comp '>'  Plus                                { KT.KtPsiGt  $1 $3 }
  | Comp '<'  Plus                                { KT.KtPsiLt  $1 $3 }
  | Comp '>=' Plus                                { KT.KtPsiGte $1 $3 }
  | Comp '<=' Plus                                { KT.KtPsiLte $1 $3 }
  | Plus                                          { $1                }

Plus
  : Plus '+' Mult                                 { KT.KtPsiAddition  $1 $3 }
  | Plus '-' Mult                                 { KT.KtPsiDifferent $1 $3 }
  | Mult                                          { $1                      }

Mult
  : Mult '*' Unary                                { KT.KtPsiMultiplication $1 $3 }
  | Mult '/' Unary                                { KT.KtPsiRatio          $1 $3 }
  | Unary                                         { $1                           }

Unary
  : '!' Unary                                     { KT.KtPsiNot    $2 }
  | '-' Unary                                     { KT.KtPsiNegate $2 }
  | Target                                        { $1                }


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
  : NAME '(' ')' '!' '!'                          { KT.KtPsiCallFun0 $1       }
  | NAME '(' ')'                                  { KT.KtPsiCallFun0 $1       }
  | NAME '(' Value ')'                            { KT.KtPsiCallFun1 $1 $3    }
  | NAME '(' Value ',' Value ')'                  { KT.KtPsiCallFun2 $1 $3 $5 }

ReadVar
  : NAME                                          { KT.KtPsiReadVariable $1 }

Int
  : INT_NUM                                       {% U.checkedInt $1 }

Double
  : DBL_NUM                                       {% U.checkedDouble $1 }

String
  : STR                                           { U.updatedString $1 }

Bool
  : TRUE                                          { KT.KtPsiBool True  }
  | FALSE                                         { KT.KtPsiBool False }

Unit
  : UNIT                                          { KT.KtPsiUnit () }

Type
  : UNIT                                          { KT.KtAnyType KT.KtUnitType   }
  | INT                                           { KT.KtAnyType KT.KtIntType    }
  | DOUBLE                                        { KT.KtAnyType KT.KtDoubleType }
  | BOOL                                          { KT.KtAnyType KT.KtBoolType   }
  | STRING                                        { KT.KtAnyType KT.KtStringType }
