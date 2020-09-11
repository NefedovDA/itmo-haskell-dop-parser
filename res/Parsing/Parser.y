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
  : Return ';'                                    { $1 }

Return
  : RETURN                                        { KT.KtPsiReturn $ KT.KtPsiUnit () }
  | RETURN Expr                                   { KT.KtPsiReturn $2                }

Expr
  : Double                                        { $1 }
  | Int                                           { $1 }
  | String                                        { $1 }
  | Bool                                          { $1 }
  | Unit                                          { $1 }

Type
  : UNIT                                          { KT.KtAnyType KT.KtUnitType   }
  | INT                                           { KT.KtAnyType KT.KtIntType    }
  | DOUBLE                                        { KT.KtAnyType KT.KtDoubleType }
  | BOOL                                          { KT.KtAnyType KT.KtBoolType   }
  | STRING                                        { KT.KtAnyType KT.KtStringType }

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
