module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Fun
import qualified OptimizationTests



main :: IO ()
main = do
    runTestTT OptimizationTests.tl
    return ()

 
