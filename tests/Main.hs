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
import qualified SimpleTests    
import qualified ClosureTests   
import qualified FactorialTests 
import qualified RecursionTests 
import qualified OptimizationTests



main :: IO ()
main = do
    runTestTT SimpleTests.tl
    runTestTT ClosureTests.tl
    runTestTT FactorialTests.tl
    runTestTT RecursionTests.tl
    runTestTT OptimizationTests.tl
    return ()
    --let
    --    fails = foldr  (\r f -> f + (failures r )) 0 [s,c,f,r]
    --if fails > 0 then Exit.exitFailure else Exit.exitSuccess
 
 

