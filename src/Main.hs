module Main where

import SECD

main = do
    txt <- getContents
    let term = parse txt
        instr  = compile  term
        
    putStrLn $ show $ term
    putStrLn $ show $ instr
    return ()
