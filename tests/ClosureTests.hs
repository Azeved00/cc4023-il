module ClosureTests where
import SECD
import Fun
import Test.HUnit

test1 :: Test
test1 = 
    let
        input   = "let x = 42 in (\\y.x+y)"
        message = "closures test 1" 
        output = Let "x" (Const 42) (Lambda "y" (Var "x" :+ Var "y"))
    in
    TestCase $ assertEqual message output (parse input)

test2 :: Test
test2 = 
    let
        input   = "let x = 23 in (\\y.x+y)"
        message = "closure test 2" 
        output = Let "x" (Const 23) (Lambda "y" (Var "x" :+ Var "y"))

    in
    TestCase $ assertEqual message output (parse input)

tl = TestList [TestLabel "closure test 1" test1,TestLabel "closure test 2" test2]



