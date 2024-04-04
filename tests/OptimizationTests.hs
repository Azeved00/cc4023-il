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


 
tl = TestList [ test1, test2, test3 ]
