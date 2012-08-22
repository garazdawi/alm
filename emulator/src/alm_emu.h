/*
 * alm_emu.h
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */

#ifndef ALM_EMU_H_
#define ALM_EMU_H_

#include "alm_loader.h"
#include "alm_term.h"

int process_main(process_t *c_p,code_t *code, ATERM funcname, ATERM *args, int arg_len);

#endif /* ALM_EMU_H_ */
