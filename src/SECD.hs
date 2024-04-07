module SECD where

import Fun
import qualified Lexer (alexScanTokens)
import qualified Parser (parse)
import qualified SECD1 
import qualified SECD2


lexer s = Lexer.alexScanTokens s 
parse s = Parser.parse s


secd1Compile  t     = SECD1.compileMain t 
secd1Optimize i     = SECD1.optimize i
secd1Execute i      = SECD1.run i
secd1Debug code     = SECD1.debug code


secd2RunState t     = SECD2.runCodeGen t
secd2Compile t      = SECD2.compileExpr t
secd2Optimize t     = SECD2.optimize t 
