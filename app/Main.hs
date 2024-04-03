module Main where

import System.Console.Haskeline
import Control.Monad.Catch
import Control.Exception (SomeException)
import SECD

mainLoop::String -> InputT IO()
mainLoop str = do
    let 
        tokens  = lexer str
        term    = parse tokens
        instr   = compile  term
    outputStrLn $ show $ tokens
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
                        $  getInputLine ("\ESC[33m\STX" ++ show n ++ " -> " ++ "\ESC[0m\STX")--λ
            case minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just s -> do
                            catch (mainLoop s) handler
                            loop (n+1)
        handler :: SomeException -> InputT IO ()
        handler ex = do
                        outputStrLn $ "\ESC[0;41m\STXCaught exception:\ESC[0m\STX" 
                        outputStrLn $ show $ ex
