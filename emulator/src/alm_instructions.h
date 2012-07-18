/*
 * alm_instructions.h
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */

#ifndef ALM_INSTRUCTIONS_H_
#define ALM_INSTRUCTIONS_H_

#include <stdint.h>

#define INSTR_COUNT 24

#define I_MOVE_XX   0
#define I_MOVE_XY   1
#define I_MOVE_YX   2
#define I_LOAD      3
#define I_FUNC      4
#define I_ADD       5
#define I_DIV       6
#define I_MUL       7
#define I_SUB       8
#define I_RET       9
#define I_JUMP      10
#define I_BRT       11
#define I_CALL      12
#define I_LABEL     13
#define I_EQ        20
#define I_NEQ       21
#define I_LT        22
#define I_GT        23

#define INSTR_iABC 0
#define INSTR_iABx 1

static const char* instruction_to_string[] =
	{ "move_xx", "move_xy", "move_yx", "load", "func", "add", "div", "mul",
		"sub", "ret", "jmp", "brt", "call", "lbl", NULL, NULL, NULL,
		NULL, NULL, NULL, "eq", "neq", "lt", "gt" };
static const int instruction_type[] = { INSTR_iABC, INSTR_iABC, INSTR_iABC,
	INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC,
	INSTR_iABC, INSTR_iABx, INSTR_iABx, INSTR_iABC, INSTR_iABx, INSTR_iABC,
	INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC,
	INSTR_iABC, INSTR_iABC, INSTR_iABC };

#ifndef INSTR_FIRST
#define GET_INSTR(IP) ((*(IP)) >> 26)
#define GET_A(I) (((I) >> 18) & ((1 << 8) - 1))
#define GET_B(I) (((I) >> 9) & ((1 << 9) - 1))
#define GET_C(I) ((I) & ((1 << 9) - 1))
#define GET_Bx(I) ((I) & ((1 << 18) - 1))
#define SET_Bx(I, val) I = (((I) | ((1 << 18) - 1)) & (val | 0xFFFC0000))
#else
#define GET_INSTR(IP) (*(IP)) & ((1 << 6) - 1)
#define GET_A(I) (I >> 6) & ((1 << 8) - 1)
#define GET_B(I) (I >> 14) & ((1 << 9) - 1)
#define GET_C(I) (I >> 23) & ((1 << 9) - 1)
#define GET_Bx(I) (I >> 14) & ((1 << 18) -1)
#endif

#define GET_iABC(IP,A,B,C) \
    do { uint32_t tmp = *(IP); \
	 A =  GET_A(tmp);\
	 B = GET_B(tmp); \
	 C = GET_C(tmp); \
    } while(0)
#define GET_iABx(IP,A,Bx) \
    do { uint32_t tmp = *(IP); \
	 A = GET_A(tmp); \
	 Bx= GET_Bx(tmp); \
    } while(0)

#endif /* ALM_INSTRUCTIONS_H_ */
