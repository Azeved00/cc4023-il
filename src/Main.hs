module Main where

import System.Console.Haskeline
import SECD

mainLoop::String -> InputT IO()
mainLoop str = do
    let 
        term = parse str
        instr  = compile  term
    outputStrLn $ show $ term
    outputStrLn $ show $ instr
    return()

mySettings :: Settings IO
mySettings = defaultSettings {historyFile = Just "myhist"}

main :: IO ()
main = do
        runInputT mySettings $ withInterrupt $ loop 0
    where
        loop :: Int -> InputT IO ()
        loop n = do
            minput <-  handleInterrupt (return (Just "Caught interrupted"))
                        $  getInputLine (show n ++ " -> ")--λ
            case minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just s -> do
                            mainLoop s
                            loop (n+1)
