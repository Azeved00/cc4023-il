{
module Parser where

import Fun
}

%name parse
%tokentype { Token }
%error { parseError }

%token

var         { VAR $$ }
const       { CONST $$ }
'+'         { OP PLUS }
'-'         { OP MINUS}
'*'         { OP MULT }
lambda      { LAMBDA }
'.'         { DOT }
let         { LET }
'='         { EQUAL }
in          { IN }
ifzero      { IFZERO }
fix         { FIX }
'('         { LPARENT }
')'         { RPARENT }

%left '+' '-'
%left '*'

%%

Term : var                      { Var $1 }
     | '(' Term ')'             { $2 }
     | lambda var '.' Term      { Lambda $2 $4}
     | Term Term                { App $1 $2 }
     | const                    { Const $1 }
     | Term '+' Term            { $1 :+ $3 }
     | Term '-' Term            { $1 :- $3 }
     | Term '*' Term            { $1 :* $3 }
     | ifzero Term Term Term    { IfZero $2 $3 $4 }
     | let var '=' Term in Term { Let $2 $4 $6 }
     | fix Term                 { Fix $2 }

{
parseError :: [Token] -> a
parseError toks = error ("parse error" ++ (show (head toks)) ++ " " ++ show (take 10 toks) ++ " " ++ show (length toks))
}
