/*
 * alm_term.h
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */

#ifndef ALM_TERM_H_
#define ALM_TERM_H_

#include <stdint.h>

typedef union { uint64_t bin; double num; } ATERM;

#include "alm_mem.h"

// All primary data types

#define mk_non_num(x) (ATERM)(((ATERM)(x)).bin & 0x000FFFFFFFFFFFFF)
#define is_not_number(x) (((((x).bin) & 0x7FF0000000000000) == 0) && (x).bin > 1 )

/* all number are doubles */
#define is_num(x) (((((x).bin) & 0x7FF0000000000000) != 0) || (x).bin < 2 )
#define mk_num(x) (ATERM)(((ATERM)(x)).bin ^ 0x7FF0000000000000)
#define num_val(x) (mk_num(x)).num

/* header word used to tag a pointer on stack or a boxed something on heap */
#define is_header(x) (((x).bin >> 48) == 0 && (x).bin > 1)
#define mk_header(x) (ATERM)((mk_non_num((uint64_t)x)).bin & 0x0000FFFFFFFFFFFF)
#define header_val(x) (x).bin

/* nil */
#define is_nil(x) (((x).bin >> 48) == 2 && (x).bin > 1)
#define mk_nil()  (ATERM)(uint64_t)0x0002000000000000LL

/* cons cell */
#define is_cons(x) (((x).bin >> 48) == 3 && (x).bin > 1)
#define mk_cons(x) (ATERM)((mk_non_num((uint64_t)x)).bin | 0x0003000000000000)
#define cons_ptr(x) ((ATERM*)((x).bin & 0x0000FFFFFFFFFFFF))
#define CONS_heap(heap, aterm, head, tail)	\
    do { \
        ATERM *cell = Halloc(heap,2);		\
	cell[0] = head; \
	cell[1] = tail; \
	aterm = mk_cons(cell);\
    } while(0);
#define CONS(aterm, head, tail) CONS_heap(c_p->h, aterm, head, tail)
#define CAR(x) (*(cons_ptr(x)))
#define CDR(x) (*(cons_ptr(x)+1))

/* Stack frames pushed to stack on function call */
#define is_frame(x) is_header(x)
#define mk_frame(x) mk_header(x)
#define frame_val(x) header_val(x)

/* Pointer to a boxed datatype, i.e. atom etc */
#define is_boxed(x) (((x).bin >> 48) == 1 && (x).bin > 1)
#define mk_boxed(x) (ATERM)((mk_non_num((uint64_t)x)).bin | 0x0001000000000000)
#define boxed_ptr(x) ((ATERM*)((x).bin & 0x0000FFFFFFFFFFFF))
#define boxed_arity(x) ((x).bin & 0x00007FFFFFFFFFFF)

// All boxed data types

/* A boxed atom */
#define is_atom(x) (is_header(x) && ((x).bin >> 47) == 1)
#define tag_atom(x) (ATERM)((mk_header(x)).bin | 0x0000800000000000)
#define mk_atom_heap(heap, aterm, str, len)	\
    do {\
        ATERM *atom = Halloc(heap,2+len / sizeof(ATERM) + 1);	\
	atom[0] = tag_atom(1+len / sizeof(ATERM) + 1); \
	atom[1] = (ATERM)(uint64_t)len;		       \
	strncpy((char*)(atom+2),(str),(len));\
	aterm = mk_boxed(atom); \
    } while(0)
#define mk_atom(aterm, str, len) mk_atom_heap(c_p->h, aterm, str, len)

#endif /* ALM_TERM_H_ */
