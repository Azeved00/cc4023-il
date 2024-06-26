module Main where

import System.Environment
import System.Console.Haskeline
import Control.Monad.Catch
import Control.Monad
import Control.Exception (SomeException)
import SECD

type ProgSettings = (Bool,Bool,Bool,Bool,Bool)

mainLoop::String -> ProgSettings -> InputT IO()
mainLoop str (l,p,c,o,e)= 
    let 
        tokens  = lexer str
        term    = parse tokens
        instr   = secd1Compile  term
        oinstr  = secd1Optimize instr
        trace   = secd1Debug    (if o then oinstr else instr) 
    in do
    if l then outputStrLn $ show $ tokens   else outputStr "" 
    if p then outputStrLn $ show $ term     else outputStr ""
    if c then outputStrLn $ show $ instr    else outputStr ""
    if o then outputStrLn $ show $ oinstr   else outputStr ""
    if e then mapM_ (\x -> outputStrLn $ show $ x) trace
        else outputStr ""
    return()
    

rlSettings :: Settings IO
rlSettings = defaultSettings {historyFile = Just "myhist"}



main :: IO ()
main = do
        args <- getArgs
        let setting = (False,False,True,True,False)
        runInputT rlSettings $ withInterrupt $ loop 0 setting
    where
        loop :: Int -> ProgSettings -> InputT IO ()
        loop n (l,p,c,o,e) = do
            minput <-  handleInterrupt (return (Just "Caught interrupted"))
                        $  getInputLine ("\ESC[33m\STX" ++ show n ++ " -> " ++ "\ESC[0m\STX")--λ
            case minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just ":l" -> loop (n+1) (not l,p,c,o,e)
                Just ":p" -> loop (n+1) (l,not p,c,o,e)
                Just ":c" -> loop (n+1) (l,p,not c,o,e)
                Just ":o" -> loop (n+1) (l,p,c,not o,e)
                Just ":e" -> loop (n+1) (l,p,c,o,not e)
                Just s -> do
                            catch (mainLoop s (l,p,c,o,e)) handler
                            loop (n+1) (l,p,c,o,e)
        handler :: SomeException -> InputT IO ()
        handler ex = do
                        outputStrLn $ "\ESC[0;41m\STXCaught exception:\ESC[0m\STX" 
                        outputStrLn $ show $ ex
