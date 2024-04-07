module OptimizationTests where
import Fun
import SECD 
import SECD1 
import Test.HUnit


test1 :: Test
test1 = 
    let
        input   = "\\x.ifzero 1 2 ((\\x.x) 10)"
        message = "Non optimized conditionals"
        output  =   [LDF 
                        [LDC 1,
                        SEL [LDC 2,JOIN] 
                            [LDF [LD 0,RTN],LDC 10,AP,JOIN],RTN],
                    HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd1Compile (SECD.parse $ SECD.lexer $ input))

test2 :: Test
test2 = 
    let
        input   = "\\x.ifzero 1 2 10"
        message = "Optimized conditionals"
        output  =   [LDF 
                        [LDC 1,
                        TEST [LDC 2,RTN],
                        LDC 10,
                        RTN]
                    ,HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd1Optimize $ secd1Compile (SECD.parse $ SECD.lexer $ input))

test3 :: Test
test3 = 
    let
        input   = "\\x.ifzero 1 2 ((\\x.x) 10)"
        message = "Optimized conditionals & Direct Application"
        output  =   [LDF 
                        [LDC 1,
                        TEST [LDC 2,RTN],
                        LDC 10,
                        AA,
                        LD 0,
                        RTN]
                    ,HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd1Optimize $ secd1Compile (SECD.parse $ SECD.lexer $ input))

test4 :: Test
test4 = 
    let
        input   = "(\\x.x+1) 4"
        message = "Optimized closures"
        output  =   [LDC 4,
                    AA,
                    LD 0,
                    LDC 1,
                    ADD,
                    HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output
        (secd1Optimize $ secd1Compile (SECD.parse $ SECD.lexer $ input))

test4' :: Test
test4' = 
    let
        input   = "let e=1 in (e+1)"
        message = "Optimized let closures "
        output  =   [LDC 1, 
                    AA,
                    LD 0,
                    LDC 1,
                    ADD,
                    HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output
        (secd1Optimize $ secd1Compile (SECD.parse $ SECD.lexer $ input))

test5 :: Test
test5 = 
    let
        input   = "(fix \\f.\\x.ifzero x 1 (f (x-1))) 10"
        message = "Full Tail Recursion"
        output  =   [LDRF 
                        [LD 0,
                        TEST [LDC 1,RTN],
                        LD 1,
                        LD 0,
                        LDC 1,
                        SUB,
                        TRAP],
                    LDC 10,
                    AP,
                    HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output 
        (secd1Optimize $ secd1Compile (SECD.parse $ SECD.lexer $ input))



 
tl = TestList [ test1, test2, test3, test4, test4', test5]
