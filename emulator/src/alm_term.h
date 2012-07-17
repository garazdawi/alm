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
#define is_number(x) (((((x).bin) & 0x7FF0000000000000) != 0) || (x).bin < 2 )
#define mk_num(x) (ATERM)(((ATERM)(x)).bin ^ 0x7FF0000000000000)
#define num_val(x) (mk_num(x)).num

/* Stack frames pushed to stack on function call */
#define is_frame(x) ((x) > 1 && ((x) >> 48) == 0)
#define frame_val(x) (x).bin
#define mk_frame(x) (ATERM)((mk_non_num((uint64_t)x)).bin & 0x0000FFFFFFFFFFFF)

#endif /* ALM_TERM_H_ */
