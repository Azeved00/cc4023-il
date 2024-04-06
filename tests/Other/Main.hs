module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Fun
import qualified FactorialTests 



main :: IO ()
main = do
    runTestTT FactorialTests.tl
    return ()

 

