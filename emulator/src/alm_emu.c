#include <stdio.h>
#include "alm_emu.h"
#include "alm_instructions.h"
#include "alm_loader.h"
#include "alm_debug.h"

//#define HARD_DEBUG
#ifdef HARD_DEBUG
#define DEBUG
#define DBG(...) alm_printf( __VA_ARGS__ )
#else
#define DBG(...)
#endif

int process_main(process_t *c_p,code_t* code, ATERM funcname, ATERM *args, int arg_len) {
    INSTR *I;
    ATERM *S;
#define NUM_XREG 32
    ATERM reg_x[NUM_XREG];

    int A, B, C, i;

    I = code->instructions;

#ifdef DEBUG
    for (i = 0; i < NUM_XREG; i++)
    	reg_x[i] = (ATERM)0ull;
#endif

    for (i = 0; i < arg_len; i++)
      reg_x[i] = args[i];

    while (1) {
#define S (c_p->h.stack)
#define iABC_CASE(LBL, CODE) case LBL: { GET_iABC(I, A, B, C); DBG("%-7s %.3d %.3d %.3d ",instruction_to_string[LBL],A,B,C); CODE; I++; break; }
#define iABx_CASE(LBL, CODE) case LBL: { GET_iABx(I, A, B); DBG("%-7s %.3d %.3d     ",instruction_to_string[LBL],A,B); CODE; I++; break; }
	switch (GET_INSTR(I)) {
iABC_CASE(I_MOVE_XX,reg_x[B] = reg_x[A])
iABC_CASE(I_MOVE_XY,S[B+1] = reg_x[A])
iABC_CASE(I_MOVE_YX,reg_x[B] = S[A+1])
iABC_CASE(I_LOAD,reg_x[B] = code->constants[A])
iABC_CASE(I_FUNC,*S = mk_frame(I))
iABC_CASE(I_RET,for (i = 1; i < NUM_XREG; i++) reg_x[i] = (ATERM)0ull; if (S == c_p->h.top) goto done; I = (INSTR*)frame_val(*S); do { S--; } while(!is_frame(*S)) )
iABx_CASE(I_BRT,if (num_val(reg_x[A]) == 0.0) I += B)
iABx_CASE(I_JUMP,I += B)
case I_CALL: {
    GET_iABC(I, A, B, C);
    DBG("%-7s %.3d %.3d %.3d ",instruction_to_string[I_CALL],A,B,C);
    function_t *f = code->func_list;
    while (f->constant != A && f != NULL)
	f = f->next;
    CHK(f == NULL);
    S += C + 1;
    *S = mk_frame(I);
    I = f->instruction;
#ifdef DEBUG
    /* Clear x registers so that debug printing is nicer! */
    for (i = B; i < NUM_XREG; i++)
	reg_x[i] = (ATERM)0ull;
#endif
    I++;
    break;
}
case I_LABEL: I++; continue;
iABC_CASE(I_CONS,CONS(reg_x[C],reg_x[A],reg_x[B]))
iABC_CASE(I_GC,GC_CHECK(A,B+1,C))
iABC_CASE(I_ADD,reg_x[C] = mk_num(num_val(reg_x[A]) + num_val(reg_x[B])))
iABC_CASE(I_SUB,reg_x[C] = mk_num(num_val(reg_x[A]) - num_val(reg_x[B])))
iABC_CASE(I_MUL,reg_x[C] = mk_num(num_val(reg_x[A]) * num_val(reg_x[B])))
iABC_CASE(I_DIV,reg_x[C] = mk_num(num_val(reg_x[A]) / num_val(reg_x[B])))

iABC_CASE(I_EQ,reg_x[C] = mk_num((double)(num_val(reg_x[A]) == num_val(reg_x[B]))))
iABC_CASE(I_NEQ,reg_x[C] = mk_num((double)(num_val(reg_x[A]) != num_val(reg_x[B]))))
iABC_CASE(I_LT,reg_x[C] = mk_num((double)(num_val(reg_x[A]) < num_val(reg_x[B]))))
iABC_CASE(I_GT,reg_x[C] = mk_num((double)(num_val(reg_x[A]) > num_val(reg_x[B]))))
		    default:
			alm_printf("Illigal instruction %d\r\n",GET_INSTR(I));
			CHK(1); break;
		}
	for (i=0;i<10;i++)
	    if (reg_x[i].bin != 0)
		DBG("%T ",reg_x[i]);
	    else
		DBG("N/A ");
	DBG("\r\n");
    }
    done:
       alm_printf("%T\r\n",reg_x[0]);
       return 0;

    return 0;
}
