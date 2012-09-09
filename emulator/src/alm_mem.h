/*
 * alm_mem.h
 *
 *  Created on: Aug 17, 2012
 *      Author: lukas
 */

#ifndef ALM_MEM_H_
#define ALM_MEM_H_

#include "alm_term.h"

#define HEAP_START_SIZE 10

#define Halloc(P_H,words) ((P_H).heap -= words, (P_H).heap)
#define GC_CHECK(xlive,ylive,needed) if (c_p->h.stack+needed+ylive >= c_p->h.heap) alm_gc(c_p,needed,reg_x,xlive,ylive)
#define INIT_HEAP(P) init_heap(&(P)->h,HEAP_START_SIZE)


typedef struct heap {
    ATERM* stack;
    ATERM* heap;
    ATERM* top;
    uint32_t size;
} heap_t;

int init_heap(heap_t *h, uint32_t size);

#endif /* ALM_MEM_H_ */
