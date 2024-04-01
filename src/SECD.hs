module SECD where

import Fun
import qualified Lexer (alexScanTokens)
import qualified Parser (parse)
import qualified SECD1 (compileMain,Instr)


parse :: String -> Term
parse s = 
    let 
        lexer = Lexer.alexScanTokens s 
    in
    Parser.parse $ lexer

compile :: Term -> [SECD1.Instr]
compile t = SECD1.compileMain t

