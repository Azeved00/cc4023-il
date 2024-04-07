module OptimizationTests where

import Fun
import SECD 
import SECD2 
import Test.HUnit
import qualified Data.Map as Map


test1 :: Test
test1 = 
    let
        input   = "\\x.ifzero 1 2 ((\\x.x) 10)"
        message = "Non optimized conditionals"
        output  =  Map.fromList [
                    ("_main",[LDF "l3",HALT]),
                    ("l0",[LD 0,RTN]),
                    ("l1",[LDC 2,JOIN]),
                    ("l2",[LDF "l0",LDC 10,AP,JOIN]),
                    ("l3",[LDC 1,SEL "l1" "l2",RTN])]

    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd2RunState $ secd2Compile (parse $ lexer $ input))

test2 :: Test
test2 = 
    let
        input   = "\\x.ifzero 1 2 10"
        message = "Optimized conditionals"
        output  = Map.fromList [
                    ("_main",[LDF "l2",HALT]),
                    ("l0",[LDC 2,RTN]),
                    --("l1",[LDC 10,JOIN]),
                    ("l2",[LDC 1,TEST "l0",LDC 10, RTN])]
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd2Optimize $ secd2Compile (parse $ lexer $ input))

test3 :: Test
test3 = 
    let
        input   = "\\x.ifzero 1 2 ((\\x.x) 10)"
        message = "Optimized conditionals & Direct Application"
        output  = Map.fromList [
                    ("_main",[LDF "l3",HALT]),
                    --("l0",[LD 0,RTN]),
                    ("l1",[LDC 2,RTN]),
                    --("l2",[LDC 10,AA,LD 0,RTN]),
                    ("l3",[LDC 1,TEST "l1",LDC 10,AA,LD 0,RTN])]
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd2Optimize $ secd2Compile (parse $ lexer $ input) )

test4 :: Test
test4 = 
    let
        input   = "(\\x.x+1) 4"
        message = "Optimized closures"
        output  = Map.fromList [("_main",[LDC 4,AA,LD 0,LDC 1,ADD,HALT])]
    in
    TestLabel message $ TestCase $ assertEqual message output
        (secd2Optimize $ secd2Compile (parse $ lexer $ input))

test4' :: Test
test4' = 
    let
        input   = "let e=1 in (e+1)"
        message = "Optimized let closures "
        output  = Map.fromList [("_main",[LDC 1,AA,LD 0,LDC 1,ADD,HALT])]
    in
    TestLabel message $ TestCase $ assertEqual message output
        (secd2Optimize $ secd2Compile (parse $ lexer $ input))

test5 :: Test
test5 = 
    let
        input   = "(fix \\f.\\x.ifzero x 1 (f (x-1))) 10"
        message = "Full Tail Recursion"
        output  = Map.fromList [
                    ("_main",[LDRF "l2",LDC 10,AP,HALT]),
                    ("l0",[LDC 1,RTN]),
                    --("l1",[LD 1,LD 0,LDC 1,SUB,AP,JOIN]),
                    ("l2",[LD 0,TEST "l0",LD 1,LD 0,LDC 1,SUB,TRAP])]
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd2Optimize $ secd2Compile (parse $ lexer $ input))

 
tl = TestList [ test1, test2, test3, test4, test4', test5 ]
