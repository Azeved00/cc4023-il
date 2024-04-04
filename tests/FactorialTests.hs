module FactorialTests where
import SECD
import Fun
import Test.HUnit

fact = Fix 
      (Lambda "f" 
       (Lambda "n"
        (IfZero (Var "n")
         (Const 1)
         ((App (Var "f") (Var "n" :- Const 1)) :* Var "n")
        )))

-- an example that builds a closure with free vars
test1 :: Test
test1 = 
    let    
        input   = "fix (\\f.\\n.ifzero n 1 ((f (n - 1))*n))"
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


tl = TestList [ TestLabel "factorial definition" test1, 
                TestLabel "factorial of 10" test2, 
                TestLabel "factorial of -1" test3 ]
