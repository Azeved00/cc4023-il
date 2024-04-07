{-# LANGUAGE DeriveFunctor #-}
{- -----------------------------------
   Fun: a minimal functional language
   -----------------------------------
   A byte-code compiler for a SECD-like virtual machine.
   
   Pedro Vasconcelos, 2008--2020.
 -}
module SECD2 where
import           Fun
import           Data.List (elemIndex)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

-----------------------------------------------------------------
-- SECD machine definitions
-----------------------------------------------------------------

-- pseudo instructions parameterized by label type
data Instr l = HALT            -- finished
             | LDC Int         -- load constant
             | LD Int          -- load variable
             | ADD             -- addition
             | SUB             -- subtraction
             | MUL             -- multiplication
             | SEL l l         -- select zero/non-zero
             | LDF l           -- load a closure
             | LDRF l          -- load a recursive closure
             | AP              -- apply
             | RTN             -- return 
             | JOIN            -- close branch
             | TEST l          -- test branch
             | DAP             -- direct apply
             | AA              -- add arguments
             | TRAP            -- Full Tail Recursion
             deriving (Show, Functor, Eq)

-- symbolic labels are just strings
type Label = String

-- a block of (symbolic code) 
type Block = [Instr Label]

-- a State monad for generating fresh labels and storing code blocks
type CodeGen = State (Map Label Block)

-- add a new code block segment
-- returns the new generated label
newBlock :: Block -> CodeGen Label
newBlock code = do
  table <- get
  let label = "l" ++ show (Map.size table) 
  put (Map.insert label code table)
  return label



-- compile a Fun term into SECD code
compile :: Term -> [Ident] -> CodeGen Block
compile (Var x) sym 
    = case elemIndex x sym of
        Nothing -> error ("free variable: " ++ show x)
        Just k ->  return [LD k]
-- "elemIndex x xs" 
-- gives the index of first occurence of x in xs or Nothing 

compile (Lambda x e) sym 
  = do code <- compile e (x:sym) 
       label <- newBlock (code++[RTN])
       return [LDF label]

-- compile a recursive function
compile (Fix (Lambda f (Lambda x e1))) sym
  = do code <- compile e1 (x:f:sym) 
       label <- newBlock (code++[RTN])
       return [LDRF label]

compile (App e1 e2) sym 
  = do code1 <- compile e1 sym 
       code2 <- compile e2 sym 
       return (code1 ++ code2 ++ [AP])
       
compile (Const n) sym
  = return [LDC n]

compile (e1 :+ e2) sym 
  = do code1 <- compile e1 sym 
       code2 <- compile e2 sym 
       return (code1++code2 ++ [ADD])

compile (e1 :- e2) sym 
  = do code1 <- compile e1 sym
       code2 <- compile e2 sym 
       return (code1 ++ code2 ++ [SUB])

compile (e1 :* e2) sym 
  = do code1 <- compile e1 sym 
       code2 <- compile e2 sym 
       return (code1 ++ code2 ++ [MUL])

compile (IfZero e1 e2 e3) sym
  = do code1 <- compile e1 sym  
       code2 <- compile e2 sym
       code3 <- compile e3 sym
       ltrue <- newBlock (code2 ++ [JOIN])
       lfalse<- newBlock (code3 ++ [JOIN])
       return (code1 ++ [SEL ltrue lfalse])

compile (Let x e1 e2) sym
    = compile (App (Lambda x e2) e1) sym
 

-- compile a top-level expression
compileExpr :: Term -> CodeGen Block
compileExpr e = do
  code <- compile e [] 
  return (code ++ [HALT])

-- run a code generator
-- entry point begins at label "_main"
-- note: it should be the first label in sorting order
runCodeGen :: CodeGen Block -> Map Label Block
runCodeGen cgen =  Map.insert "_main" code0 labels  
  where (code0, labels) = runState cgen Map.empty


-----------------------------------------------------------------
-- Optimize SECD Code
-----------------------------------------------------------------
-- Simulate  the stack to figure out 
-- if a recursive call occurs in last AP (if it exists)
-- that is if, before the RTN instruction: 
-- it is an AP and
-- the second element in the stack is the adress of the function we are evaluating
data Stack  = I Int 
            | A Int
            deriving(Eq,Show)

checkTR' :: ([Stack], Block) -> ([Stack], Block)
checkTR' (s,LD x:xs)        = (A x:s,xs)
checkTR' (s,LDC x:xs)       = (I x:s,xs)
checkTR' (_:_:s,ADD:xs)     = (I 0:s,xs)
checkTR' (_:_:s,SUB:xs)     = (I 0:s,xs)
checkTR' (_:_:s,MUL:xs)     = (I 0:s,xs)
checkTR' (_:_:s,AP:xs)      = (I 1:s,xs)
checkTR' (_:_:s,DAP:xs)     = (I 0:s,xs)
checkTR' (_:s,SEL _ _:xs)   = (s,xs)
checkTR' (_:s,TEST _:xs)    = (s,xs)
checkTR' (s,RTN:c)          = error ("checkTR': undefined for RTN" )
checkTR' (s,x:xs)           = (s,xs)
checkTR' conf               = error ("checkTR': undefined for " ++ show conf)

checkTR :: Block -> Bool
checkTR code = (length s >= 2 && head (tail s) == A 1)
    where   confs = iterate checkTR' ([],code)
            trace = takeWhile (not.final) confs
            (s, c) = last trace
            final (s, c)  
                | null c        = True
                | c == []       = True
                | head c == RTN = True
                | otherwise     = False

optimizeBlock ::  Map Label Block -> Block -> Label -> (Map Label Block, Block)
-- Optimization 1: simpler conditionals
optimizeBlock tab ((SEL lt lf):RTN:xs) l = 
    let
        fct = mapaux $ Map.lookup lt tab
        (tab',ofct) = optimizeBlock tab ((init fct) ++ [RTN]) lt  -- substitute join with rtn
        tab1 = Map.insert lt ofct tab'                    -- update code

        fcf = mapaux $ Map.lookup lf tab1
        (tab1',ofcf) = optimizeBlock tab1 ((init fcf)++[RTN]) lf --take join out and then optimize
        tab2 = Map.delete lf tab1'              --delete because it will be inlined

        (ftab,result) = optimizeBlock tab2 xs l
    in (ftab,[TEST lt] ++ ofcf ++ result)

-- Optimization 4: full tail recursion
optimizeBlock tab (LDRF fl:xs) l =
    let
        fc = mapaux $ Map.lookup fl tab 
        (tab', ofc) = optimizeBlock tab fc fl
        tr = checkTR ofc
        code = if tr then (takeUntilAP ofc) ++ [TRAP] else ofc  
        tab1 = Map.insert fl code tab'
        (ftab, result) = optimizeBlock tab1 xs l
        takeUntilAP (AP:RTN:[]) = []
        takeUntilAP (DAP:[]) = []
        takeUntilAP ([]) = []
        takeUntilAP (x:xs) = x:(takeUntilAP xs)
    in (ftab,[LDRF fl] ++ result)

-- Optimization 3: Avoid Extra Closures
optimizeBlock tab (LDF fl:x:AP:xs) l = 
    let
        fc = mapaux $ Map.lookup fl tab
        -- remove RTN from function code and then optimize
        (tab', ofc) = optimizeBlock tab (init fc) fl   
        -- delete beacuse it will be inlined
        tab1 = Map.delete fl tab'              
        -- parameter of function goes to start
        (ftab,result) = optimizeBlock tab1 xs l
    in (ftab,[x] ++ [AA] ++ ofc ++ result)

-- Optimization 2: direct application
optimizeBlock tab (AP:RTN:xs) l  = let
        (ftab,result) = optimizeBlock tab xs l
    in (ftab, [DAP] ++ result)

-- Normal traversal
optimizeBlock tab (SEL ct cf:xs) l = 
    let
        fc1 =mapaux $ Map.lookup ct tab 
        (tab',codet) = optimizeBlock tab fc1 ct
        tab1 = Map.insert ct codet tab'

        fc2 = mapaux $ Map.lookup cf tab1 
        (tab1', codef) = optimizeBlock tab1 fc2 cf
        tab2 = Map.insert cf codef tab1'

        (ftab,result) = optimizeBlock tab2 xs l
    in (ftab,[SEL ct cf] ++ result)

optimizeBlock tab (LDF x:xs) l = 
    let
        fc = mapaux $ Map.lookup x tab 
        (tab', code) = optimizeBlock tab fc x
        tab1 = Map.insert x code tab'
        
        (ftab,result) = optimizeBlock tab1 xs l
    in (ftab,[LDF x] ++ result)
optimizeBlock tab (x:xs) l  = 
    let
    (ftab,result) = optimizeBlock tab xs l
    in (ftab,[x] ++ result)
optimizeBlock tab [] _ = (tab,[])

optimize :: CodeGen Block -> Map Label Block
optimize s = 
    let
        table = runCodeGen s
        mainc = mapaux $ Map.lookup "_main" table
        (tab,code) = optimizeBlock table mainc "_main"
        ftab = Map.insert "_main" code tab
    in ftab
        
mapaux Nothing = []
mapaux (Just c) = c

-----------------------------------------------------------------------------
-- resolving labels
-----------------------------------------------------------------------------
-- code addresses are simple integers
type Addr = Int

-- assign each label to a bytecode address
resolveLabels :: Map Label Block -> Map Label Addr
resolveLabels table
  = Map.fromList (zip labels addrs)
  where
    labels = Map.keys table
    sizes = map (\code -> sum (map sizeof code)) (Map.elems table)
    addrs = scanl (+) 0 sizes

-- flatten labeled blocks into a list of instructions
flattenCode :: Map Label Block -> [Instr Addr]
flattenCode table
  = map patch $ concat (Map.elems table)
  where addrs = resolveLabels table
        patch = fmap (\l -> Map.findWithDefault undefined l addrs)


----------------------------------------------------------------------------
-- assemblying into bytecodes
----------------------------------------------------------------------------
-- bytecodes are just fixed size integers
type Bytecode = Int

-- "assemble" a single instruction to bytecode
asmInstr :: Instr Addr -> [Bytecode]
asmInstr HALT        = [0]
asmInstr (LDC n)     = [1, n]
asmInstr (LD n)      = [2, n]
asmInstr ADD         = [3]
asmInstr SUB         = [4]
asmInstr MUL         = [5]
asmInstr (SEL l1 l2) = [6, l1, l2]
asmInstr (LDF l)     = [7, l]
asmInstr (LDRF l)    = [8, l]
asmInstr AP          = [9]
asmInstr RTN         = [10]
asmInstr JOIN        = [11]
asmInstr (TEST l)    = [12, l]
asmInstr DAP         = [13]
asmInstr AA          = [14]
asmInstr TRAP        = [15]

-- assemble a code block  
asmCode :: [Instr Addr] ->  [Bytecode]
asmCode = concatMap asmInstr

-- number of bytecodes for each instruction
sizeof :: Instr l -> Int
sizeof instr = case instr of
  SEL _ _ -> 3
  LD _ -> 2
  LDC _ -> 2
  LDF _ -> 2
  LDRF _ -> 2
  TEST _ -> 2
  _ -> 1


-------------------------------------------------------------------
-- putting it all together
-------------------------------------------------------------------

-- generate bytecode for a closed term
compileBytecode :: Term -> [Bytecode]
compileBytecode
  = asmCode . flattenCode . runCodeGen  . compileExpr

-- write bytecode one item per line
showBytecode :: [Bytecode] -> String
showBytecode code = unlines $ map show code

-- compile a closed term to a bytecode file
compileToFile :: FilePath -> Term -> IO ()
compileToFile path expr
  = writeFile path $ showBytecode $ compileBytecode expr
  

