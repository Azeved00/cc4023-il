## Example code for Language implementation course (DCC/FCUP)

1. Abstract syntax for FUN, a small functional language
2. Three interpreters in Haskell (Eval1, Eval2, Eval3)
3. Two compilers in Haskell (SECD1, SECD2)
4. SECD code interpreter in Haskell (SECD2)
5. SECD code interpreter in C (secd.c)

## Syntax of expressions
### Grammar
```
Lamb : lambda var '.' Lamb      
     | Term

Term : Term '+' Term        
     | Term '-' Term         
     | Term '*' Term           
     | ifzero Atom Atom Atom   
     | let var '=' Atom in Atom
     | fix Lamb                
     | Apply                   

Apply: Apply Atom 
     | Atom       

Atom : var        
     | const 
     | '(' Lamb ')' 
```

### Examples
Expressions let 

    let x = 4

Lambda expressions

    \x.x 

Conditionals

    ifzero 1 2 3

Applications

    (\x.x) 4

Recursive functions

    fix \f.\z.ifzero x 1 (f (x-1))

##  Running the code

### For SECD1
Running the tests:

    cabal test SECD1 --test-show-details=streaming

Running in repl (read-eval-print-loop)

    cabal run SECD1

repl has the commands

Lexer       - `:l` 

Parser      - `:p`

Compiler    - `:c`

Optimizer   - `:o`

Interpreter - `:e`

Each command is meant to be given alone and,
following the command, 
the output associated function will be displayed.

By default both the compiler and 
the optimizer output are displayed.

### For SECD2
Running the optimizer tests:

    cabal test SECD2 --test-show-details=streaming
Running the interpreter tests:

     ./tests/Interp/interp.sh

Running in repl mode:

    cabal --enable-nix run SECD2

Optimizing a single expression expr

    cabal --enable-nix run SECD2 -- "expr"

Optimizing an expression and putting it in file "a"

    cabal --enable-nix run SECD2 -- -o a "expr"


repl has the commands

Lexer       - `:l` 

Parser      - `:p`

Compiler    - `:c`

Optimizer   - `:o`

Each command is meant to be given alone and,
following the command, 
the output associated function will be displayed.

By default only the optimizer output is displayed



### For C interpreter
Compiling the binary:

     make secd
Running it by giving it a file "a" 

     bin/secd < a

### For dumpless C interpreter
Compiling the binary:

     make sec
Running it by giving it a file "a" 

     bin/sec < a

## Assigment

1. Adding a parser for Fun Language (using Alex/Happy)
2. Optimize SECD machine by extendim the compiler and C interpreter to
    - Avoid Un-necessary construction of closures for let-expressions
    - Simplify conditional instructions in tail call position
    - Last call optimization
    - tail recursion optimization, aka avoid growing the stack when recusive call occurs in tail poisition
    - Combine the stack and dump of the C implementation into a single stack
3. Provide examples of effects of optimizations


----
Special thanks to:
Pedro Vasconcelos, 2024
