/*
 * alm_debug.c
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */
#include <stdio.h>

#include "alm_debug.h"
#include "alm_instructions.h"

int alm_disasm(code_t* code) {
    int i, instr, a, b, c;

    printf("Constants: %d\r\n", code->num_constants);
    for (i = 0; i < code->num_constants; i++) {
	printf("  const[%d] : %d\r\n", i, code->constants[i]);
    }

    printf("Instructions: %d\r\n", code->num_instructions);
    for (i = 0; i < code->num_instructions; i++) {
	instr = GET_INSTR(code->instructions+i);
	if (instr < 0 || instr >= INSTR_COUNT)
	    printf("??%.3X?", instr);
	else
	    printf("  %-4s", instruction_to_string[instr]);
	if (instruction_type[instr] == INSTR_iABC) {
	    GET_iABC(code->instructions+i, a, b, c);
	    printf(" %.3d %.3d %.3d\r\n", a, b, c);
	}
    }
    return 1;
}
