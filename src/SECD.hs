module SECD where

import Fun
import qualified Lexer (alexScanTokens)
import qualified Parser (parse)
import qualified SECD1 (compileMain,Instr)


lexer s = Lexer.alexScanTokens s 

parse s = Parser.parse $ s

compile :: Term -> [SECD1.Instr]
compile t = SECD1.compileMain t

