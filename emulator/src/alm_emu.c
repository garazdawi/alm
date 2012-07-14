#include <stdio.h>
#include "alm_emu.h"
#include "alm_instructions.h"
#include "alm_loader.h"

int process_main(code_t* code) {
    uint32_t *I;
    uint64_t *S;
    uint64_t reg_x[32];
    uint64_t reg_y[255];

    int A, B, C;

    I = code->instructions;
    S = reg_y;

    while (1) {
	GET_iABC(I, A, B, C);
	switch (GET_INSTR(I)) {
		    case I_MOVE: {
			reg_x[B] = reg_x[A];
			break;
		    }
		    case I_LOAD: {
			reg_x[B] = code->constants[A];
			break;
		    }
		    case I_FUNC: {
			*S = (uint64_t)I+1;
			S+=2;
			break;
		    }
		    case I_RET: {
			S-=2;
			if (S == reg_y) {
			    printf("%lld\r\n",reg_x[0]);
			    return 0;
			} else {
			    I = (uint32_t*)*S;
			}
			break;
		    }
		    case I_ADD: {
			reg_x[C] = reg_x[A] + reg_x[B];
			break;
		    }
		    case I_SUB: {
			reg_x[C] = reg_x[A] - reg_x[B];
			break;
		    }
		    case I_MUL: {
			reg_x[C] = reg_x[A] * reg_x[B];
			break;
		    }
		    case I_DIV: {
			reg_x[C] = reg_x[A] / reg_x[B];
			break;
		    }
		}
	I++;
    }

    return 0;
}
