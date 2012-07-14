/*
 * alm_debug.h
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */

#ifndef ALM_DEBUG_H_
#define ALM_DEBUG_H_

#include "alm_loader.h"

#define DEBUG 1

#define CHK(expr) do { if (expr) { printf("Failed on line %d\r\n",__LINE__); exit(1); } } while(0)

int alm_disasm(code_t* code);

#endif /* ALM_DEBUG_H_ */
