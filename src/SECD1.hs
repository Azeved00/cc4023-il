{- -----------------------------------
   Fun: a minimal functional language
   -----------------------------------
   A compiler and interpreter for a SECD-like virtual machine.
   
   Pedro Vasconcelos, 2008--2011.
 -}
module SECD1 where
import Fun
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------
-- SECD machine definitions
-----------------------------------------------------------------

-- pseudo instructions 
data Instr = HALT            -- finished
           | LDC Int         -- load constant
           | LD Int          -- load variable
           | ADD             -- addition
           | SUB             -- subtraction
           | MUL             -- multiplication
           | SEL [Instr] [Instr] -- select zero/non-zero
           | JOIN            -- close branch
           | LDF [Instr]     -- load a closure
           | LDRF [Instr]    -- load a recursive closure
           | AP              -- apply
           | RTN             -- return 
           | TEST [Instr]    -- test branch
           | DAP             -- direct apply
           | AA              -- add arguments
           | TRAP            -- Full Tail Recursion
            deriving(Show,Eq)

-- a code block (list of instructions)
type Code = [Instr]


-- closure: pairs of code, environment
type Closure = (Code, Env)

-- closure addresses
type Addr = Int

-- store for closures
type Store = Map Addr Closure

-- get the next available address
nextAddr :: Store -> Addr
nextAddr store = 1 + Map.size store

-- a value of the SECD machine is either
-- a primitive integer or the address of a closure
data Value = I Int
           | A Addr
             deriving (Eq,Show)

-- the SECD machine components
type Stack = [Value]

type Env   = [Value]

type Dump  = [(Stack,Env,Code)]

-- the SECD machine configuration
type Conf  = (Stack, Env, Code, Dump, Store) 


-----------------------------------------------------------------
-- EXECUTE SECD Code
-----------------------------------------------------------------
-- execute a single SECD instruction
execute :: Conf -> Conf

execute (stack, env, (LDC n):code, dump, store) 
    = (I n:stack, env, code, dump, store)

execute (I v2:I v1:stack, env, ADD:code, dump, store)
    = (I (v1+v2):stack, env, code, dump, store)

execute (I v2:I v1:stack, env, SUB:code, dump, store)
    = (I (v1-v2):stack, env, code, dump, store)

execute (I v2:I v1:stack, env, MUL:code, dump, store)
    = (I (v1*v2):stack, env, code, dump, store)

execute (stack, env, LD i:code, dump, store)
    = let v = env!!i
      in (v:stack, env, code, dump, store)

execute (stack, env, LDF code':code, dump, store)
    = let addr = nextAddr store
          store'= Map.insert addr (code',env) store
      in (A addr:stack, env, code, dump, store')

execute (stack, env, LDRF code':code, dump, store)
    = let addr= nextAddr store
          store'=Map.insert addr (code', A addr:env) store
      in (A addr:stack, env, code, dump, store')


execute (arg:A addr:stack, env, AP:code, dump, store)
    = let Just (code',env')= Map.lookup addr store
      in ([], arg:env', code', (stack,env,code):dump, store)

execute (v:stack, env, RTN:code, (stack',env',code'):dump, store)
    = (v:stack', env', code', dump, store)


execute (I n:stack, env, (SEL code1 code2):code, dump,store)
    | n==0      = (stack, env, code1, ([],[],code):dump, store)
    | otherwise = (stack, env, code2, ([],[],code):dump, store)

execute (stack, env, JOIN:code, (_,_,code'):dump, store)
    = (stack, env, code', dump, store)

execute (stack, env, HALT:code, dump, store)
    = (stack, env, [], dump, store)

-- Implementation Of Optimizations
execute (I n:stack, env, (TEST code1):code, dump,store)= let
        c = if n == 0 then code1 else code
    in (stack, env, c, dump, store)

execute (arg:A addr:stack, env, DAP:code, dump, store)
    = let Just (code',env')= Map.lookup addr store
      in ([], arg:env', code', dump, store)

execute (arg:A addr:stack, env, TRAP:code, dump, store)
    = let Just (code',env')= Map.lookup addr store
      in ([],arg:A addr:stack, code', dump, store)

execute (x:stack, env, AA:code, dump,store)
    = (stack, x:env, code, dump, store)

execute conf
    = error ("execute: undefined for " ++ show conf)


-- execution trace starting from an initial state
executeT :: Conf -> [Conf]
executeT conf = trace
    where confs = iterate execute conf
          trace = takeWhile (not.final) confs
          final (s, e, c, d, st) = null c 


-- run a sequence of machine instructions
-- returns the result value 
run :: Code -> Value
run code = value
    where trace = executeT ([],[],code,[],Map.empty)
          (value:_, _, _, _, _) = last trace

debug :: Code -> [Conf]
debug code = executeT ([],[],code,[],Map.empty)
          

-- valor, máximo comprimento de pilha e do dump
runStats :: Code -> (Value, Int, Int)
runStats code = (value, maxstack, maxdump)
  where trace = executeT ([],[],code,[],Map.empty)
        (value:_, _, _, _, _) = last trace
        maxstack = maximum [length s | (s,_,_,_,_)<-trace]
        maxdump  = maximum [length d | (_,_,_,d,_)<-trace]



-----------------------------------------------------------------
-- Compile lambda term into SECD Code
-----------------------------------------------------------------
compile (Var x) sym 
    = case elemIndex x sym of
        Nothing -> error ("free variable: " ++ show x)
        Just k -> [LD k]
-- "elemIndex x xs" 
-- gives the index of first occurence of x in xs or Nothing 

compile (IfZero e1 e2 e3) sym
  = let code1 = compile e1 sym 
        code2 = compile e2 sym ++ [JOIN]
        code3 = compile e3 sym ++ [JOIN]
    in code1 ++ [SEL code2 code3]

compile (Lambda x e) sym 
  = let code = compile e (x:sym) ++ [RTN]
    in [LDF code]

-- compile a recursive function
compile (Fix (Lambda f (Lambda x e1))) sym
  = let code = compile e1 (x:f:sym) ++ [RTN]
    in [LDRF code]

compile (App e1 e2) sym 
  = let code1 = compile e1 sym 
        code2 = compile e2 sym 
    in code1 ++ code2 ++ [AP]
       
compile (Const n) sym = [LDC n]

compile (e1 :+ e2) sym 
  = let code1= compile e1 sym 
        code2= compile e2 sym 
    in code1 ++ code2 ++ [ADD]

compile (e1 :- e2) sym 
  = let code1=compile e1 sym
        code2=compile e2 sym 
    in code1 ++ code2 ++ [SUB]

compile (e1 :* e2) sym 
  = let code1 = compile e1 sym 
        code2 = compile e2 sym 
    in code1 ++ code2 ++ [MUL]

compile (Let x e1 e2) sym
    = compile (App (Lambda x e2) e1) sym
 

-- compile the main expression
compileMain :: Term ->  [Instr]
compileMain e = compile e [] ++ [HALT]


-----------------------------------------------------------------
-- Optimize SECD Code
-----------------------------------------------------------------

checkTR' :: Conf -> Conf
checkTR' (s,e,LD x:xs,d,m) = (A x:s,e,xs,d,m)
checkTR' (s,e,LDC x:xs,d,m) = (I x:s,e,xs,d,m)
checkTR' (_:_:s,e,ADD:xs,d,m) = (I 0:s,e,xs,d,m)
checkTR' (_:_:s,e,SUB:xs,d,m) = (I 0:s,e,xs,d,m)
checkTR' (_:_:s,e,MUL:xs,d,m) = (I 0:s,e,xs,d,m)
checkTR' (_:A f:s,e,AP:xs,d,m) = (I 1:s,e,xs,d,m)
checkTR' (_:A f:s,e,DAP:xs,d,m) = (I 0:s,e,xs,d,m)
checkTR' (_:s,e,SEL _ _:xs,d,m) = (s,e,xs,d,m)
checkTR' (_:s,e,TEST _:xs,d,m) = (s,e,xs,d,m)
checkTR' (s,e,RTN:c,d,m) = error ("checkTR': undefined for " ++ show RTN)
checkTR' (s,e,x:xs,d,m) = (s,e,xs,d,m)
checkTR' conf  = error ("checkTR': undefined for " ++ show conf)

checkTR :: [Instr] -> Bool
checkTR code = (length s >= 2 && head (tail s) == A 1)
    where   confs = iterate checkTR' ([],[],code,[],Map.empty)
            trace = takeWhile (not.final) confs
            (s, e, c, d, m) = last trace
            final (s, e, c, d, m)  
                | null c        = True
                | c == []       = True
                | head c == RTN = True
                | otherwise     = False

optimize :: [Instr] -> [Instr]
-- Optimization 1: simpler conditionals
-- Note: init is O(n), use Sequences to get better performance
optimize (SEL ct cf:RTN:xs) = let
        ct'   = init ct   
        cf'   = init cf   
        codeT = optimize (ct'++[RTN])
        codeF = optimize (cf'++[RTN])
    in [TEST codeT] ++ codeF ++ optimize xs  

-- Optimization 4: full tail recursion
optimize (LDRF fc:xs)  = let
        ofc = optimize fc 
        tr = checkTR ofc
        fcode = if tr then init ofc ++ [TRAP] else ofc
    in [LDRF fcode] ++ optimize xs  

-- Optimization 3: Avoid Extra Closures
optimize (LDF fc:x:AP:xs)  = let
        code = optimize (init fc) 
    in [x] ++ [AA] ++ code ++ optimize xs  



-- Optimization 2: direct application
optimize (AP:RTN:xs)  = let
    in [DAP] ++ optimize xs  

-- Normal traversal
optimize (SEL ct cf:xs)  = let
        codeT = optimize ct 
        codeF = optimize cf 
    in [SEL codeT codeF] ++ optimize xs 

optimize (LDF x:xs)  = let 
        code = optimize x 
    in [LDF code] ++ optimize xs 

optimize (x:e)  = [x] ++ optimize e 
optimize []  = []
