/*
 * A bytecode interpreter for a SECD-like machine 
 * Pedro Vasconcelos <pbv@dcc.fc.up.pt>, 2024
 */
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "secd.h"

#define DEBUG 0

typedef struct {
    value_t v;
    dump_t d;
} s_elem;

int     *code;       /* code segment */
s_elem *stack;

/* Stack Code */
void print_stack(int sp){
    if(sp == 0){
        printf("Stack: empty");
        return;
    }
    s_elem x = stack[sp-1];
    if(x.d.pc == -1)
        printf("Stack: %ld", x.v);
    else if (x.d.env != NULL)
        printf("Stack: (%d,%ld)", x.d.pc, GET_ELM(x.d.env));
    else 
        printf("Stack: (%d,NULL)", x.d.pc);
}
void push_value(int* sp, value_t v){
    s_elem e;
    e.v = v;
    e.d.pc = -1;
    e.d.env = NULL;
    stack[(*sp)++] = e;
}
value_t pop_value(int* sp){
    return stack[--(*sp)].v;
}
void push_dump(int* sp, int pc ,env_t env){
    s_elem e;
    e.v = -1;
    e.d.pc = pc;
    e.d.env = env;
    stack[(*sp)++] = e;
}
dump_t pop_dump(int* sp){
    dump_t top = stack[--(*sp)].d;
    while(top.pc == -1){
        top = stack[--(*sp)].d;
    }
    return top;
}

/* Base Code  */
void init_segments(int code_size, int stack_size, int dump_size) {
    code = (int*) malloc(code_size*sizeof(int));
    stack = (s_elem*)malloc(stack_size*sizeof(s_elem));
    assert(code != NULL);
    assert(stack != NULL);
}

void free_segments(void) {
    free(code);
    free(stack);
}


/* extend an environment with a new value, i.e.
   cons a value on to a list 
*/
env_t extend(value_t elm, env_t env) {
    env_t nenv = alloc_cell();
    SET_ELM(nenv, elm);
    SET_NEXT(nenv, env);
    return nenv;
}

/* lookup a value in an environment by index 
*/
value_t lookup(int n, env_t env) {
    while(n > 0) {
        assert(env != NULL);
        env = GET_NEXT(env);
        n--;
    }
    return GET_ELM(env);
}

/* allocate a new closure 
*/
closure_t *mkclosure(int pc, env_t env) {
    closure_t *ptr = alloc_cell();
    SET_CODE(ptr,pc);
    SET_ENV(ptr,env);
    return ptr;
}


/* byte code interpretation loop
 */
value_t interp(void) {
    int pc = 0;          // program counter
    int sp = 0;          // stack pointer 
    env_t env = NULL;    // environment pointer

    for (;;) {            // loop
        value_t opa, opb;   // temporary operands
        int t;              // temporary register
        closure_t *cptr;    // closure pointer
        env_t nenv;         // temporary environment
        dump_t dump;

        int opcode = code[pc++];    // fetch next opcode
        if (DEBUG){
            if (env == NULL){
                print_stack(sp);
                printf(", opcode: %d, env: NULL \n",opcode);
            }
            else {
                print_stack(sp);
                printf(", opcode: %d, env: %ld \n", opcode, GET_ELM(env));
            }
        }

        switch(opcode) {
        // Optimizations
        case TEST:
            opa = pop_value(&sp);               // integer on top of stack
            if (opa == 0) pc = code[pc];
            else pc++;
            break;
        case AA:
            opa = pop_value(&sp);               // function argument
            env = extend(opa, env);             // put it in env
            break;
        case DAP:
            opa = pop_value(&sp);               // function argument
            cptr = (closure_t*) pop_value(&sp); // function closure
            env = extend(opa, GET_ENV(cptr));   // augment environment
            pc = GET_CODE(cptr);                // jump to code address in closure
            break;
        case TRAP:
            opa = pop_value(&sp);               // function argument
            cptr = (closure_t*)pop_value(&sp);  // function closure
            SET_ELM(env,opa);                   // substitute top of env with top of stack
            pc = GET_CODE(cptr);                // jump to code address in closure
            break;

        // Base Code
        case LDC: 
            opa = (value_t)code[pc++];      // fetch operand
            push_value(&sp,opa);
            break;
        case LD: 
            t = code[pc++];       // fetch index
            push_value(&sp, lookup(t,env));
            break;
        case ADD: 
            opa = pop_value(&sp); 
            opb = pop_value(&sp);  
            push_value(&sp,opa + opb);
            break;
        case SUB: 
            opa = pop_value(&sp); 
            opb = pop_value(&sp);  
            push_value(&sp,opb - opa); // order
            break;
        case MUL: 
            opa = pop_value(&sp); 
            opb = pop_value(&sp);  
            push_value(&sp,opa * opb);
            break;
        case SEL:
            opa = pop_value(&sp);  // integer on top of stack
            
            push_dump(&sp, pc+2, NULL);
            if (opa == 0) 
                pc = code[pc];
            else 
                pc = code[pc+1];
            break;
        case LDF: 
            t = code[pc++];                     // fetch code address
            cptr = mkclosure(t, env);           // make a new closure
            push_value(&sp, (value_t)cptr);
            break;
        case LDRF: 
            t = code[pc++];                     // fetch code address
            nenv = extend((value_t)NULL, env);
            cptr = mkclosure(t, nenv);
            SET_ELM(nenv,cptr);                 // tie the knot in the environment
            push_value(&sp, (value_t) cptr);
            break;
        case AP:
            opa = pop_value(&sp);               // function argument
            cptr = (closure_t*) pop_value(&sp); // function closure
            push_dump(&sp, pc, env);
            env = extend(opa, GET_ENV(cptr));   // augment environment
            pc = GET_CODE(cptr);                // jump to code address in closure
            break;
        case RTN:
            opa = pop_value(&sp);               // get what is at top of stack
            dump = pop_dump(&sp);               // pop what was left until a dump is reached
            pc = dump.pc;                       // restore PC
            env = dump.env;                     // restore enviroment
            push_value(&sp,opa);                // put top back as return
            break;

        case JOIN:
            opa = pop_value(&sp);           //get what is at top of stack
            dump = pop_dump(&sp);           // pop what was left until a dump is reached
            pc = dump.pc;                   // restore PC
            push_value(&sp,opa);            // put top back as return
            break;

        case HALT:
            return pop_value(&sp);          // return top of stack 

        default:
            fprintf(stderr, "invalid opcode %d at program counter %d\n", opcode, pc);
            exit(-1);
        }

        /* check register bounds */
        assert(sp>=0 && sp<STACK_MAX);
        assert(pc>=0 && pc<CODE_MAX);
    }
}



/* read bytecode into memory; returns number of entries read
 */
int read_code(FILE *f) {
    int i = 0, data;

    while(!feof(f) && i<CODE_MAX) {
        if(fscanf(f, "%d\n", &data)!=1) return 0;
        code[i++] = data;
    }
    return i;
}


int main(int argc, char *argv[]) {
    FILE *file = stdin;
    if (argc >= 2) {
        char *filename = argv[1];
        FILE *file = fopen(filename, "r");
        if (file == NULL) {
            perror("Error opening file");
            return 1; // Return non-zero to indicate error
        }
    }

    value_t top;  /* top of stack */

    /* allocate segments and heap
    */
    init_segments(CODE_MAX, STACK_MAX, DUMP_MAX);
    init_heap(HEAP_MAX);
    read_code(file);
    top = interp();
    printf("%d\n", (int)top);

    /* clean-up 
    */
    free_segments();
    free_heap();
    return 0;
}
