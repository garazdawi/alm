#include <stdio.h>
#include "alm_emu.h"
#include "alm_instructions.h"
#include "alm_loader.h"
#include "alm_debug.h"

int process_main(code_t* code, ATERM *args, int arg_len) {
    INSTR *I;
    ATERM *S;
    ATERM reg_x[32];
    ATERM reg_y[255];

    int A, B, C, i;

    I = code->instructions;
    S = reg_y;

    for (i = 0; i < arg_len; i++)
      reg_x[i] = args[i];

    while (1) {
#define iABC_CASE(LBL, CODE) case LBL: { GET_iABC(I, A, B, C); CODE; break; }
#define iABx_CASE(LBL, CODE) case LBL: { GET_iABx(I, A, B); CODE; break; }
	switch (GET_INSTR(I)) {
iABC_CASE(I_MOVE_XX,reg_x[B] = reg_x[A])
iABC_CASE(I_MOVE_XY,S[B] = reg_x[A])
iABC_CASE(I_LOAD,reg_x[B] = code->constants[A])
iABC_CASE(I_FUNC,*S = mk_frame(I+1); S+=2)
iABC_CASE(I_RET,S-=2;
		if (S == reg_y) {
		    printf("%lf\r\n",num_val(reg_x[0]));
		    return 0;
		} else {
		    I = (INSTR*)frame_val(*S);
		})
iABC_CASE(I_ADD,reg_x[C] = mk_num(num_val(reg_x[A]) + num_val(reg_x[B])))
iABC_CASE(I_SUB,reg_x[C] = mk_num(num_val(reg_x[A]) - num_val(reg_x[B])))
iABC_CASE(I_MUL,reg_x[C] = mk_num(num_val(reg_x[A]) * num_val(reg_x[B])))
iABC_CASE(I_DIV,reg_x[C] = mk_num(num_val(reg_x[A]) / num_val(reg_x[B])))

iABC_CASE(I_EQ,reg_x[C] = mk_num((double)(num_val(reg_x[A]) == num_val(reg_x[B]))))
iABC_CASE(I_NEQ,reg_x[C] = mk_num((double)(num_val(reg_x[A]) != num_val(reg_x[B]))))
iABC_CASE(I_LT,reg_x[C] = mk_num((double)(num_val(reg_x[A]) < num_val(reg_x[B]))))
iABC_CASE(I_GT,reg_x[C] = mk_num((double)(num_val(reg_x[A]) > num_val(reg_x[B]))))
		    default:
			CHK(0); break;
		}
	I++;
    }

    return 0;
}
