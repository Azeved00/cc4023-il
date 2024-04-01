module Main where
import Lexer
import Parser
import SECD1


main = do
    txt <- getContents

    let lexer = alexScanTokens txt
        pars  = parse $ lexer
        comp  = compileMain $ pars 
        
    putStrLn $ show $ pars
    putStrLn $ show $ comp
    return ()
