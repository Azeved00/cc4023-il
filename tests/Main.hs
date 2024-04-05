{--------------------------------------
  Fun: a minimal functional language
  -------------------------------------
 
  This module contains some example programs.
  Pedro Vasconcelos, 2008--2009.
-}
module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Fun
import qualified ParserTests
import qualified FactorialTests 
import qualified OptimizationTests



main :: IO ()
main = do
    runTestTT ParserTests.tl
    runTestTT OptimizationTests.tl
    runTestTT FactorialTests.tl
    return ()
    --let
    --    fails = foldr  (\r f -> f + (failures r )) 0 [s,c,f,r]
    --if fails > 0 then Exit.exitFailure else Exit.exitSuccess
 
 

