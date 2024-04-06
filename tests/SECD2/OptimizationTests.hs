module OptimizationTests where
import Fun
import qualified SECD 
import SECD2 
import Test.HUnit

{-
test1 :: Test
test1 = 
    let
        input   = "\\x.ifzero 1 2 ((\\x.x) 10)"
        message = "Non optimized conditionals"
        output  =   []
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (compileExpr (SECD.parse $ SECD.lexer $ input))

test2 :: Test
test2 = 
    let
        input   = "\\x.ifzero 1 2 10"
        message = "Optimized conditionals"
        output  =   []
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (compileExpr (SECD.parse $ SECD.lexer $ input))

test3 :: Test
test3 = 
    let
        input   = "\\x.ifzero 1 2 ((\\x.x) 10)"
        message = "Optimized conditionals & Direct Application"
        output  =   []
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (compileExpr (SECD.parse $ SECD.lexer $ input) )

test4 :: Test
test4 = 
    let
        input   = "(\\x.x+1) 4"
        message = "Optimized closures"
        output  =   []
    in
    TestLabel message $ TestCase $ assertEqual message output
        (compileExpr (SECD.parse $ SECD.lexer $ input))

test4' :: Test
test4' = 
    let
        input   = "let e=1 in (e+1)"
        message = "Optimized let closures "
        output  =   []
    in
    TestLabel message $ TestCase $ assertEqual message output
        (compileExpr (SECD.parse $ SECD.lexer $ input))

test5 :: Test
test5 = 
    let
        input   = "(fix \\f.\\x.ifzero x 1 (f (x-1))) 10"
        message = "Full Tail Recursion"
        output  =   []
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (compileExpr (SECD.parse $ SECD.lexer $ input))

-}

 
tl = TestList [ ]
