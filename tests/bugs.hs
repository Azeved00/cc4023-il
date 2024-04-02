
import Test.HUnit
-- buggy expressions (type errors)

bug1 = Const 42 :+ Lambda "x" (Var "x")

bug2 = App (Const 42) (Const 1)


