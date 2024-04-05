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
    TestLabel message $ TestCase $ assertEqual message output (SECD.compile (parse $ lexer $ input) False)

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
    TestLabel message $ TestCase $ assertEqual message output (SECD.compile (parse $ lexer $ input) True)



test3 :: Test
test3 = 
    let
        input   = "\\x.ifzero 1 2 ((\\x.x) 10)"
        message = "Optimized conditionals & Direct Application"
        output  =   [LDF 
                        [LDC 1,
                        TEST [LDC 2,RTN],
                        LDF [LD 0,RTN],
                        LDC 10,
                        DAP]
                    ,HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output (SECD.compile (parse $ lexer $ input) True)

test4 :: Test
test4 = 
    let
        input   = "\\f.\\x.(x+1)"
        message = "Optimized closures"
        output  =   [LDF 
                        [AA,
                        LD 0,
                        LDC 1,
                        ADD,
                        RTN]
                    ,HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output (SECD.compile (parse $ lexer $ input) True)

test5 :: Test
test5 = 
    let
        input   = "fix \\f.\\x.ifzero x 1 (f (x-1))"
        message = "Optimized closures"
        output  =   [LDRF 
                        [LD 0,
                        TEST [LDC 1,RTN],
                        LD 1,
                        LD 0,
                        LDC 1,
                        SUB,
                        TRAP]
                    ,HALT]
    in
    TestLabel message $ TestCase $ assertEqual message output (SECD.compile (parse $ lexer $ input) True)



 
tl = TestList [ test1, test2, test3, test4, test5]
