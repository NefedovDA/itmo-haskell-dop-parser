{
module Parsing.Parser
  ( happyParserExpression
  ) where

import qualified Parsing.Helper    as H
import qualified Parsing.KotlinPsi as KT
import qualified Parsing.Result    as R
import qualified Parsing.Token     as T
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
  : FunDecl                                       { KT.KtPsiFile $ H.unproxyFunDec $1 }

FunDecl
  : Fun0 FunDecl                                  {% H.putFun0Data $1 $2 }
  | Fun1 FunDecl                                  {% H.putFun1Data $1 $2 }
  | Fun2 FunDecl                                  {% H.putFun2Data $1 $2 }
  |                                               {  H.emptyProxyFunDec  }

Fun0
  : FUN NAME '(' ')' ':' Type '{' '}'             { KT.KtPsiFun0 $2 $6 }

Fun1
  : FUN NAME '(' Arg ')' ':' Type '{' '}'         { KT.KtPsiFun1 $2 $4 $7 }

Fun2
  : FUN NAME '('Arg ',' Arg ')' ':' Type '{' '}'  { KT.KtPsiFun2 $2 $4 $6 $9 }

Arg
  : NAME ':' Type                                 { ($1, $3) }

Type
  : UNIT                                          { KT.KtUnitType   }
  | INT                                           { KT.KtIntType    }
  | DOUBLE                                        { KT.KtDoubleType }
  | BOOL                                          { KT.KtBoolType   }
  | STRING                                        { KT.KtStringType }

Return
  : RETURN ';'                                    { KT.KtPsiReturn Kt.KtPsiUnit }
  | RETURN Expr ';'                               { KT.KtPsiReturn $2 }

Expr
  : Int                                           { $1 }
  | Double                                        { $1 }
  | String                                        { $1 }
  | Bool                                          { $1 }
  | Unit                                          { $1 }

Int
  : INT_NUM                                       {% H.checkedInt $1 }

Double
  : DBL_NUM                                       {% H.checkedDouble $1 }

String
  : STR                                           { H.updatedString $1 }

Bool
  : TRUE                                          { KT.KtPsiBool True  }
  | FALSE                                         { KT.KtPsiBool False }

Unit
  : UNIT                                          { KT.KtPsiUnit () }
