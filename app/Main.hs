module Main where

import System.Environment
import System.Console.Haskeline
import Control.Monad.Catch
import Control.Monad
import Control.Exception (SomeException)
import SECD

data ProgSettings =   Lex
                | Parse
                | Compile
                | Execute
                | Debug
                deriving(Eq)



mainLoop::String -> ProgSettings -> InputT IO()
mainLoop str set= 
    let 
        tokens  = lexer str
        term    = parse tokens
        instr   = compile term True
        res     = debug instr
    in do 
    case set of
        Lex     ->  outputStrLn $ show $ tokens
        Parse   ->  outputStrLn $ show $ term
        Compile ->  outputStrLn $ show $ instr
        Execute ->  outputStrLn "Not Implemented"
        Debug   -> do
                outputStrLn $ show $ tokens
                outputStrLn $ show $ term
                outputStrLn $ show $ instr
                mapM_ (\x -> outputStrLn $ show $ x) res
    return()
    

rlSettings :: Settings IO
rlSettings = defaultSettings {historyFile = Just "myhist"}



main :: IO ()
main = do
        args <- getArgs
        let setting = case args of
                ["lex",[l]]     -> Lex  
                ["parse",[p]]   -> Parse
                ["compile", [c]]-> Compile
                ["debug", [d]]  -> Debug
                _               -> Debug
        runInputT rlSettings $ withInterrupt $ loop 0 setting
    where
        loop :: Int -> ProgSettings -> InputT IO ()
        loop n setting = do
            minput <-  handleInterrupt (return (Just "Caught interrupted"))
                        $  getInputLine ("\ESC[33m\STX" ++ show n ++ " -> " ++ "\ESC[0m\STX")--λ
            case minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just s -> do
                            catch (mainLoop s setting) handler
                            loop (n+1) setting 
        handler :: SomeException -> InputT IO ()
        handler ex = do
                        outputStrLn $ "\ESC[0;41m\STXCaught exception:\ESC[0m\STX" 
                        outputStrLn $ show $ ex
