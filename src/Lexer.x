{
module Lexer where

import Data.Char
import Fun

}


%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z_]		-- alphabetic characters
$all = [\x00-\x10ffff]  -- all characters

tokens :-
$white+;

\-                      { \_ -> OP MINUS }
\(                      { \_ -> LPARENT }
\)                      { \_ -> RPARENT }
\*                      { \_ -> OP MULT }
\+                      { \_ -> OP PLUS }
\\                      { \_ -> LAMBDA }
\.                      { \_ -> DOT }
let                     { \_ -> LET }
\=                      { \_ -> EQUAL }
in                      { \_ -> IN }
ifzero                  { \_ -> IFZERO }
fix                     { \_ -> FIX }



$digit+                 { \s -> CONST (read s) }
$alpha($alpha|$digit)*  { \s -> VAR (map toLower s) }

{



}
