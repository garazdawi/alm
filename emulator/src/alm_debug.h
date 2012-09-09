/*
 * alm_debug.h
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */

#ifndef ALM_DEBUG_H_
#define ALM_DEBUG_H_

#include <stdlib.h>
#include <assert.h>

#include "alm_loader.h"

#define CHK(expr) do { if (expr) { printf("Failed on line %d in %s\r\n",__LINE__,__FUNCTION__); exit(1); } } while(0)

int alm_disasm(code_t* code);
int alm_dump_heap(heap_t* h,int ylive, ATERM* rootset, int rootset_size);

#endif /* ALM_DEBUG_H_ */
