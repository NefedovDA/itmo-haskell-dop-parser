{
module Parsing.Parser
  ( happyParserExpression
  ) where

import qualified Parsing.Expression   as E
import qualified Parsing.Token        as T
import           Parsing.ParserHelper (Result, parseError, returnE, thenE)
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
  : ListFun0Unit                       { E.KtPsiFile $1 }

ListFun0Unit
  : Fun0Unit ListFun0Unit              { $1 : $2 }
  |                                    { []      }

Fun0Unit
  : FUN NAME '(' ')' ':' UNIT '{' ListCallFun0 '}'  { E.KtPsiFun0Unit $2 $8 }

ListCallFun0
  : CallFun0 ListCallFun0              { $1 : $2 }
  |                                    { [] }

CallFun0
  : NAME '(' ')' ';'                   { E.KtPsiCallFun0 $1 }
