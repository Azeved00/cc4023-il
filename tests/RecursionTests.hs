module RecursionTests where
import SECD
import Fun
import Test.HUnit

-- recursive sum 0^2 + 1^2 + 2^2 + ... + n^2
test1 :: Test
test1 = 
    let
        input   = "fix\\f.\\n.ifzero (n) (0) (n*n + f (n-1))"
        message = "recursion sum" 
        output = Fix (Lambda "f"
                    (Lambda "n"
                        (IfZero (Var "n")
                            (Const 0)
                            ((Var "n" :* Var "n") :+ 
                                App (Var "f") (Var "n" :- Const 1)))))
    in
    TestCase $ assertEqual message output (parse input)
 
tl = TestLabel "recursive sum" test1
