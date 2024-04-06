module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Fun
import qualified ParserTests



main :: IO ()
main = do
    runTestTT ParserTests.tl
    return ()

 
