module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Fun
import qualified OptimizationTests
import qualified ExecutionTests



main :: IO ()
main = do
    runTestTT OptimizationTests.tl
    runTestTT ExecutionTests.tl
    return ()

 
