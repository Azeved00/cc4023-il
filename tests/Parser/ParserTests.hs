module ParserTests where
import Fun
import SECD 
import Test.HUnit

testFunc x = parse $ lexer $ x

-- simple computations
test1 :: Test
test1 = 
    let
        input   = "(42+23)*5"
        output  =  (Const 42 :+ Const 23) :* Const 5
    in
    TestCase (assertEqual "simple test 1" output (testFunc input))
 
test1' :: Test
test1' = 
    let
        input   = "5*(42+23)"
        output  =  Const 5 :* (Const 42 :+ Const 23)
    in
    TestCase (assertEqual "simple test 1.2" output (testFunc input))
 
 
-- the identity function
test2 :: Test
test2 = 
    let
        input   = "\\x.x"
        message = "Identity Funciton" 
        output  = Lambda "x" (Var "x")
    in
    TestCase $ assertEqual message output (testFunc input)
 
-- the sucessor function
test3 :: Test
test3 = 
    let
        input   = "\\x.x+1"
        message = "Successor Funciton" 
        output = Lambda "x" (Var "x" :+ Const 1)
    in
    TestCase $ assertEqual message output (testFunc input)
 
 
-- one function between two integers
test4 :: Test
test4 = 
    let
        input   = "\\x.\\y.ifzero (x-y) y x"
        message = "Funciton between two integers" 
        output = Lambda "x" 
              (Lambda "y"
               (IfZero (Var "x" :- Var "y") 
                (Var "y") (Var "x")))
    in
    TestLabel message $ TestCase $ assertEqual message output (testFunc input)

test5 :: Test
test5 = 
    let
        input   = "\\x. 1+x 1 "
        message = "Application has higher priority then sum" 
        output = Lambda "x" (Const 1 :+ (App (Var "x") (Const 1)))
    in
    TestLabel message $ TestCase $ assertEqual message output (testFunc input)
 

test6 :: Test
test6 = 
    let
        input   = "\\x. 1+x 1+2 "
        message = "Application has higher priority then sum" 
        output = Lambda "x" ((Const 1 :+ (App (Var "x") (Const 1))) :+ Const 2)
    in
    TestLabel message $ TestCase $ assertEqual message output (testFunc input)


test7 :: Test
test7 = 
    let
        input   = "let x = 42 in (\\y.(x+y))"
        message = "closures test 1" 
        output = Let "x" (Const 42) (Lambda "y" (Var "x" :+ Var "y"))
    in
    TestLabel  message $ TestCase $ assertEqual message output 
        (testFunc input)

test8 :: Test
test8 = 
    let
        input   = "fix (\\f.\\n.ifzero (n) (0) (n*n + f (n-1)))"
        message = "recursion sum" 
        output = Fix (Lambda "f"
                    (Lambda "n"
                        (IfZero (Var "n")
                            (Const 0)
                            ((Var "n" :* Var "n") :+ 
                                App (Var "f") (Var "n" :- Const 1)))))
    in
    TestCase $ assertEqual message output (testFunc input)

 
tl = TestList [ TestLabel "Simple test 1.1" test1, 
                TestLabel "Simple test 1.2" test1',
                TestLabel "Idendity Function" test2,
                TestLabel "Function between 2 ints" test3,
                test4, test5, test6, test7, test8]
