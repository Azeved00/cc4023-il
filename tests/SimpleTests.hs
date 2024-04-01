

module SimpleTests where
import Fun
import SECD (parse, compile)
import Test.HUnit

-- simple computations
test1 :: Test
test1 = 
    let
        input   = "(42+23)*5"
        output  =  (Const 42 :+ Const 23) :* Const 5
    in
    TestCase (assertEqual "simple test 1" output (parse input))
 
test1' :: Test
test1' = 
    let
        input   = "5*(42+23)"
        output  =  Const 5 :* (Const 42 :+ Const 23)
    in
    TestCase (assertEqual "simple test 1.2" output (parse input))
 
 
-- the identity function
test2 :: Test
test2 = 
    let
        input   = "\\x.x"
        message = "simple test 2: Identity Funciton" 
        output  = Lambda "x" (Var "x")
    in
    TestCase $ assertEqual message output (parse input)
 
-- the sucessor function
test3 :: Test
test3 = 
    let
        input   = "\\x.x+1"
        message = "simple test 3: Successor Funciton" 
        output = Lambda "x" (Var "x" :+ Const 1)
    in
    TestCase $ assertEqual message output (parse input)
 
 
-- one function between two integers
test4 :: Test
test4 = 
    let
        input   = "\\x.\\y.ifzero (x-y) (y) (x)"
        message = "simple test 4: Funciton between two integers" 
        output = Lambda "x" 
              (Lambda "y"
               (IfZero (Var "x" :- Var "y") 
                (Var "y") (Var "x")))
    in
    TestCase $ assertEqual message output (parse input)
 
runTests = runTestTT $ TestList [test1, test1', test2, test3]
