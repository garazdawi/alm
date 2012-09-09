/*
 * alm_mem.c
 *
 *  Created on: Aug 23, 2012
 *      Author: lukas
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "alm_debug.h"
#include "alm_mem.h"

int alm_next_heap_size(heap_t *h,uint32_t needed) {
  if (h->size * 2 > needed)
    return h->size * 2;
  else
    return h->size * 2 + needed;
}

#define IS_FORWARD(x,nh) \
  (is_header(x) &&							\
   ((header_val(x) >= (uint64_t)nh.heap)) &&				\
   ((header_val(x) < (uint64_t)(nh.top + nh.size))))

int alm_gc(heap_t *old_heap, uint32_t needed, ATERM* rootset, 
	   int rootset_size, int ylive) {
  
    int i;
    heap_t new_heap;

    /* Print old heap *
    printf("\r\nOldHeap\r\n");
    alm_dump_heap(old_heap,ylive,rootset,rootset_size);
    /* */
    init_heap(&new_heap,alm_next_heap_size(old_heap,needed));

    /* Copy all non-immidiate rootset values */
    for (i = 0; i < rootset_size; i++) {
      if (is_cons(rootset[i])) {
	ATERM *ptr = cons_ptr(rootset[i]);
	if (IS_FORWARD(ptr[0],new_heap)) {
	  rootset[i] = mk_cons(((uint64_t*)ptr[0].bin));
	} else {
	  CONS_heap(new_heap,rootset[i],ptr[0],ptr[1]);
	  ptr[0] = mk_header(cons_ptr(rootset[i]));
	}
      }
    }

    /* Copy stack items */
    new_heap.stack = new_heap.top + (uint64_t)(old_heap->stack-old_heap->top);

    /* Copy all non-immidiate stack values */
    for (i = 0; i < old_heap->stack+ylive-old_heap->top; i++) {
      if (is_cons(old_heap->top[i])) {
	ATERM *ptr = cons_ptr(old_heap->top[i]);
	if (IS_FORWARD(ptr[0],new_heap)) {
	  new_heap.top[i] = mk_cons(((uint64_t*)ptr[0].bin));
	} else {
	  CONS_heap(new_heap,new_heap.top[i],ptr[0],ptr[1]);
	  ptr[0] = mk_header(cons_ptr(new_heap.top[i]));
	}
      } else {
	new_heap.top[i] = old_heap->top[i];
      }
    }

    /* Go through everything in the heap and copy where needed */
    ATERM *curr = new_heap.top + new_heap.size, *last = new_heap.heap;

    while (curr != last) {
      curr--;
      if (is_cons(*curr)) {
	ATERM *ptr = cons_ptr(*curr);
	if (IS_FORWARD(ptr[0],new_heap)) {
	  *curr = mk_cons(((uint64_t*)ptr[0].bin));
	} else {
	  CONS_heap(new_heap,*curr,ptr[0],ptr[1]);
	  ptr[0] = mk_header(cons_ptr(*curr));
	  last-=2;
	}
      }
    }
    /* Print new heap *
    printf("\r\nNewHeap\r\n");
    alm_dump_heap(&new_heap,ylive,rootset,rootset_size); 
    /* */
    free(old_heap->top);

    old_heap->top = new_heap.top;
    old_heap->stack = new_heap.stack;
    old_heap->size = new_heap.size;
    old_heap->heap = new_heap.heap;

    return 1;
}


int init_heap(heap_t *h, uint32_t size) {
  h->top = malloc(sizeof(ATERM)*size);
  if (h->top == NULL) {
    fprintf(stderr,"Could not alloc new heap");
    exit(1);
  }
  h->stack = h->top;
  h->size = size;
  h->heap = h->top + size;
  return 1;
}
