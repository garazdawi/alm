/*
 * alm_mem.h
 *
 *  Created on: Aug 17, 2012
 *      Author: lukas
 */

#ifndef ALM_MEM_H_
#define ALM_MEM_H_

#include "alm_term.h"

#define HEAP_START_SIZE 100000

#define Halloc(words) c_p->h.heap -= words; c_p->h.heap
#define GC_CHECK(needed) if (c_p->h.stack+needed > c_p->h.heap) alm_gc(c_p,needed)
#define INIT_HEAP c_p->h.top = malloc(sizeof(ATERM)*HEAP_START_SIZE); \
    c_p->h.stack = c_p->h.top; \
    c_p->h.size = HEAP_START_SIZE; \
    c_p->h.heap = c_p->h.top + HEAP_START_SIZE


typedef struct heap {
    ATERM* stack;
    ATERM* heap;
    ATERM* top;
    uint32_t size;
} heap_t;

extern heap_t *h;

#endif /* ALM_MEM_H_ */
