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
execute (I n:stack, env, (TEST code1):code, dump,store)
    | n==0      = (stack, env, code1, ([],[],code):dump, store)
    | otherwise = (stack, env, code, dump, store)

execute (arg:A addr:stack, env, DAP:code, dump, store)
    = let Just (code',env')= Map.lookup addr store
      in ([], arg:env', code', dump, store)

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



-- compile a lambda term into SECD code
compile :: Term -> [Ident] -> [Instr]
-- 3) Simplifyinf Apply sequences 
compile (Lambda x (App c1 c2)) sym 
  = let sym' = (x:sym)
        code1 = compile c1 sym'
        code2 = compile c2 sym' 
        code =  code1 ++ code2 ++ [DAP]
    in [LDF code]

compile (Fix (Lambda f (Lambda x (App c1 c2)))) sym
  = let sym' = (x:f:sym)
        code1 = compile c1 sym'
        code2 = compile c2 sym' 
        code = code1 ++ code2 ++ [DAP]
    in [LDRF code]



-------- Normal Implementation --------------
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
compileMain :: Term -> Bool ->  [Instr]
compileMain e o =let comp =  compile e [] ++ [HALT]
                in if o then optimize comp [] else comp


-------------- OPTIMIZATIONS -----------------------
-- Otimization 1: simpler conditionals
-- Note: init is O(n), use Sequences to get better performance
optimize :: [Instr] -> [Ident]-> [Instr]
optimize (SEL ct cf:RTN:xs) sym = let
        ct'   = init ct   
        cf'   = init cf   
        codeT = optimize (ct'++[RTN]) sym
        codeF = optimize (cf'++[RTN]) sym
    in [TEST codeT] ++ codeF ++ optimize xs sym 

-- Optimization 2: direct application
optimize (AP:RTN:xs) sym = let
    in [DAP] ++ optimize xs sym 

-- Normal traversal
optimize (SEL ct cf:xs) sym = let
        codeT = optimize ct sym
        codeF = optimize cf sym
    in [SEL codeT codeF] ++ optimize xs sym

optimize (LDF x:xs) sym = let 
        code = optimize x sym
    in [LDF code] ++ optimize xs sym

optimize (LDRF x:xs) sym = let 
        code = optimize xs sym
    in [LDRF code] ++ optimize xs sym

optimize (x:e) sym = [x] ++ optimize e sym
optimize [] sym = []
