module SECD where

import Fun
import qualified Lexer (alexScanTokens)
import qualified Parser (parse)
import qualified SECD1 


execute i =  SECD1.run i

debug code = SECD1.debug code

lexer s = Lexer.alexScanTokens s 

parse s = Parser.parse s

compile :: Term -> Bool -> [SECD1.Instr]
compile t b = SECD1.compileMain t b

