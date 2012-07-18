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
#define iABC_CASE(LBL, CODE) case LBL: { GET_iABC(I, A, B, C); printf("%-7s %.3d %.3d %.3d ",instruction_to_string[LBL],A,B,C); CODE; I++; break; }
#define iABx_CASE(LBL, CODE) case LBL: { GET_iABx(I, A, B); printf("%-7s %.3d %.3d     ",instruction_to_string[LBL],A,B); CODE; I++; break; }
	switch (GET_INSTR(I)) {
iABC_CASE(I_MOVE_XX,reg_x[B] = reg_x[A])
iABC_CASE(I_MOVE_XY,S[B+1] = reg_x[A])
iABC_CASE(I_MOVE_YX,reg_x[B] = S[A+1])
iABC_CASE(I_LOAD,reg_x[B] = code->constants[A])
iABC_CASE(I_FUNC,*S = mk_frame(I))
iABC_CASE(I_RET,if (S == reg_y) goto done; I = (INSTR*)frame_val(*S); do { S--; } while(!is_frame(*S)) )
iABx_CASE(I_BRT,if (num_val(reg_x[A]) == 0.0) I += B)
iABx_CASE(I_JUMP,I += B)
case I_CALL: {
    GET_iABC(I, A, B, C);
    printf("%-7s %.3d %.3d %.3d ",instruction_to_string[I_CALL],A,B,C);
    function_t *f = code->func_list;
    while (f->constant != A && f != NULL)
	f = f->next;
    CHK(f == NULL);
    S += C + 1;
    *S = mk_frame(I);
    I = f->instruction;
    I++;
    break;
}
case I_LABEL: I++; continue;
iABC_CASE(I_ADD,reg_x[C] = mk_num(num_val(reg_x[A]) + num_val(reg_x[B])))
iABC_CASE(I_SUB,reg_x[C] = mk_num(num_val(reg_x[A]) - num_val(reg_x[B])))
iABC_CASE(I_MUL,reg_x[C] = mk_num(num_val(reg_x[A]) * num_val(reg_x[B])))
iABC_CASE(I_DIV,reg_x[C] = mk_num(num_val(reg_x[A]) / num_val(reg_x[B])))

iABC_CASE(I_EQ,reg_x[C] = mk_num((double)(num_val(reg_x[A]) == num_val(reg_x[B]))))
iABC_CASE(I_NEQ,reg_x[C] = mk_num((double)(num_val(reg_x[A]) != num_val(reg_x[B]))))
iABC_CASE(I_LT,reg_x[C] = mk_num((double)(num_val(reg_x[A]) < num_val(reg_x[B]))))
iABC_CASE(I_GT,reg_x[C] = mk_num((double)(num_val(reg_x[A]) > num_val(reg_x[B]))))
		    default:
			printf("Instruction %d\r\n",GET_INSTR(I));
			CHK(1); break;
		}
	for (i=0;i<5;i++)
	    if (reg_x[i].bin != 0)
		printf("%-6.1f ",num_val(reg_x[i]));
	    else
		printf("N/A    ");
	printf("\r\n");
    }
    done:
       printf("\r\n%lf\r\n",num_val(reg_x[0]));
       return 0;

    return 0;
}
