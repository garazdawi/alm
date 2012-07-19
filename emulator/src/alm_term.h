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

#define mk_non_num(x) (ATERM)(((ATERM)(x)).bin & 0x000FFFFFFFFFFFFF)
#define is_not_number(x) (((((x).bin) & 0x7FF0000000000000) == 0) && (x).bin > 2 )

/* all number are doubles */
#define is_num(x) (((((x).bin) & 0x7FF0000000000000) != 0) || (x).bin < 2 )
#define mk_num(x) (ATERM)(((ATERM)(x)).bin ^ 0x7FF0000000000000)
#define num_val(x) (mk_num(x)).num

/* header word used to tag a pointer on stack or a boxed something on heap */
#define is_header(x) ((x).bin > 1 && ((x).bin >> 48) == 0)
#define mk_header(x) (ATERM)((mk_non_num((uint64_t)x)).bin & 0x0000FFFFFFFFFFFF)
#define header_val(x) (x).bin

/* Stack frames pushed to stack on function call */
#define is_frame(x) is_header(x)
#define mk_frame(x) mk_header(x)
#define frame_val(x) header_val(x)

/* Pointer to a boxed datatype, i.e. atom, list etc */
#define is_boxed(x) (((x).bin >> 48) == 1)
#define mk_boxed(x) (ATERM)((mk_non_num((uint64_t)x)).bin | 0x0001000000000000)
#define boxed_val(x) ((ATERM*)((x).bin & 0x0000FFFFFFFFFFFF))

/* A boxed atom */
#define is_atom(x) (((x).bin >> 47) == 1)
#define mk_atom(x) (ATERM)((mk_non_num((uint64_t)x)).bin | 0x0000800000000000)
#define atom_size(x) ((x).bin & 0x00007FFFFFFFFFFF)


#endif /* ALM_TERM_H_ */
