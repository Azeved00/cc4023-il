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

Lamb : lambda var '.' Lamb      { Lambda $2 $4 }
     | Term                     { $1 }

Term : Term '+' Term            { $1 :+ $3 }
     | Term '-' Term            { $1 :- $3 }
     | Term '*' Term            { $1 :* $3 }
     | Apply                    { $1 }
 

Apply: Apply Atom               { App $1 $2 }
     | Atom                     { $1 }

Atom : var                      { Var $1 }
     | const                    { Const $1 }
     | ifzero Atom Atom Atom    { IfZero $2 $3 $4 }
     | let var '=' Atom in Atom { Let $2 $4 $6 }
     | fix Lamb                 { Fix $2 }
     | '(' Lamb ')'             { $2 }
     
{
parseError :: [Token] -> a
parseError toks = error ("Parse Error: @" ++ (show (head toks)) ++ " in " ++ show (take 10 toks) ++ " " ++ show (length toks))
}
