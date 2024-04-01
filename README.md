## Example code for Language implementation course (DCC/FCUP)

1. Abstract syntax for FUN, a small functional language
2. Three interpreters in Haskell (Eval1, Eval2, Eval3)
3. Two compilers in Haskell (SECD1, SECD2)
4. SECD code interpreter in Haskell (SECD2)
5. SECD code interpreter in C (secd.c)


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

Pedro Vasconcelos, 2024
