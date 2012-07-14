/*
 * alm_instructions.h
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */

#ifndef ALM_INSTRUCTIONS_H_
#define ALM_INSTRUCTIONS_H_

#include <stdint.h>

#define INSTR_COUNT 8

#define I_MOVE      0
#define I_LOAD      1
#define I_FUNC      2
#define I_ADD       3
#define I_DIV       4
#define I_MUL       5
#define I_SUB       6
#define I_RET       7

#define INSTR_iABC 0
#define INSTR_iABx 1

static const char* instruction_to_string[] = { "move", "load", "func", "add", "div",
	"mul", "sub", "ret" };
static const int instruction_type[] = { INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC,
	INSTR_iABC, INSTR_iABC, INSTR_iABC, INSTR_iABC };

#define I_MOVE_ENC      INSTR_iABC
#define I_LOAD_ENC      INSTR_iABC
#define I_FUNC_ENC      INSTR_iABC
#define I_ADD_ENC       INSTR_iABC
#define I_DIV_ENC       INSTR_iABC
#define I_MUL_ENC       INSTR_iABC
#define I_SUB_ENC       INSTR_iABC
#define I_RET_ENC       INSTR_iABC

#ifndef INSTR_FIRST
#define GET_INSTR(IP) (*(IP)) >> 26
#define GET_A(I) (I >> 18) & ((1 << 8) - 1)
#define GET_B(I) (I >> 9) & ((1 << 9) - 1)
#define GET_C(I) I & ((1 << 9) - 1)
#define GET_Bx(I) I & ((1 << 18) -1)
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
