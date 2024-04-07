module FactorialTests where
import SECD
import SECD1
import Fun
import Test.HUnit

fact = Fix 
      (Lambda "f" 
       (Lambda "n"
        (IfZero (Var "n")
         (Const 1)
         ((App (Var "f") (Var "n" :- Const 1)) :* Var "n")
        )))

--fix \f.\n.ifzero n 1 (f (n - 1)*n)
test1 :: Test
test1 = 
    let    
        input   = "fix (\\f.\\n.ifzero n 1 (f (n - 1) * n))"
        message = "factorial function" 
        output = fact
    in
    TestCase $ assertEqual message output (parse  $ lexer $ input)

test2 :: Test
test2 = 
    let
        input   = "(fix \\f.\\n.ifzero n 1 ((f (n - 1))*n)) 10"
        message = "compute the factorial of 10" 
        output = App fact (Const 10)

    in
    TestCase $ assertEqual message output (parse $ lexer $ input)

test3 :: Test
test3 = 
    let
        input   = "(fix \\f.\\n.ifzero n 1 ((f (n - 1))*n)) (0-1)"
        message = "factoial of negative number" 
        output = App fact (Const 0 :- Const 1)

    in
    TestCase $ assertEqual message output (parse $ lexer $ input)

test4 :: Test
test4 = 
    let
        input   = "(fix \\f.\\n.\\a.ifzero n a (f (n - 1) (a*n))) 10 1"
        message = "Tail Recursive factorial" 
        output =   [LDRF 
                        [LDF 
                            [LD 1,
                            TEST [LD 0,RTN],
                            LD 2,
                            LD 1,
                            LDC 1,
                            SUB,
                            AP,
                            LD 0,
                            LD 1,
                            MUL,
                            DAP],
                        RTN],
                    LDC 10,
                    AP,
                    LDC 1,
                    AP,
                    HALT]

    in
    TestLabel message $ TestCase $ assertEqual message output 
            (secd1Optimize $ secd1Compile $ parse $ lexer $ input)



test5 :: Test
test5 = 
    let
        input   = "(fix \\f.\\n.ifzero n 1 (f (n - 1)*n)) 10"
        message = "factorial of 10 non-optimized" 
        output =  SECD1.I 3628800

    in
    TestLabel message $ TestCase $ assertEqual message output 
            (secd1Execute $ secd1Compile $ parse $ lexer $ input)

test6 :: Test
test6 = 
    let
        input   = "(fix \\f.\\n.ifzero n 1 (f (n - 1)*n)) 10"
        message = "factorial of 10 optimized" 
        output =  SECD1.I 3628800

    in
    TestLabel message $ TestCase $ assertEqual message output 
            (secd1Execute $ secd1Optimize $ secd1Compile $ parse $ lexer $ input)

tl = TestList [ TestLabel "factorial definition" test1, 
                TestLabel "factorial of 10" test2, 
                TestLabel "factorial of -1" test3,
                test4, test5]

