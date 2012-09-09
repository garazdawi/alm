/*
 * alm_debug.c
 *
 *  Created on: Jul 14, 2012
 *      Author: lukas
 */
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "alm_term.h"
#include "alm_debug.h"
#include "alm_instructions.h"

int alm_print_term(ATERM t) {
    int count = 0;
    if (is_num(t))
	count += printf("%.1lf", num_val(t));
    else if (is_nil(t))
	count += printf("[]");
    else if (is_cons(t)) {
	count += printf("[");
	ATERM tmp = t;
	while (is_cons(tmp)) {
	    count += alm_print_term(CAR(tmp));
	    if (is_cons(CDR(tmp))) {
		count += printf(",");
	    } else if (!is_nil(CDR(tmp))) {
		count += printf("|");
		count += alm_print_term(CDR(tmp));
	    }
	    tmp = CDR(tmp);
	}
	count += printf("]");
    } else if (is_boxed(t)) {
	ATERM *box = boxed_ptr(t);
	if (is_atom(*box))
	    count += printf("%.*s", (int) box[1].bin, (char*) (box + 2));
    } else if (is_frame(t)) {
	count += printf("<frame/0x%.3llX>",frame_val(t));
    }
    return count;
}

int alm_printf(char *format, ...) {
    va_list argp;
    int count = 0;
    char *s = strdup(format), *p, *f;
    ATERM t;

    va_start(argp, format);

    for (f = p = s; *p != '\0'; p++) {
	if (p[0] == '%') {
	    if (p[1] == 'T') {
		p[0] = '\0';
		count += vprintf(f, argp);
		t = va_arg(argp, ATERM);
		alm_print_term(t);
		p += 2;
		f = p;
	    }
	}
    }
    count += vprintf(f, argp);
    va_end(argp);
    free(s);
    return count;
}

int alm_disasm(code_t* code) {
    int i, instr, a, b, c;

    printf("Constants: %d\r\n", code->num_constants);
    for (i = 0; i < code->num_constants; i++)
	alm_printf("  const[%d] : %T\r\n", i, code->constants[i]);

    printf("Instructions: %d\r\n", code->num_instructions);
    for (i = 0; i < code->num_instructions; i++) {
	instr = GET_INSTR(code->instructions+i);

	if (instr == I_FUNC) {
	    GET_iABC(code->instructions+i, a, b, c);
	    alm_printf("\r\n0x%.3llX func %T/%d\r\n",
		    (uint64_t) (code->instructions + i), code->constants[a], b);
	    continue;
	}

	if (instr < 0 || instr >= INSTR_COUNT)
	    printf("0x%.3llX ??%.3X?", (uint64_t) (code->instructions + i),
		    instr);
	else
	    printf("0x%.3llX  %-7s", (uint64_t) (code->instructions + i),
		    instruction_to_string[instr]);
	if (instruction_type[instr] == INSTR_iABC) {
	    GET_iABC(code->instructions+i, a, b, c);
	    printf(" %.3d %.3d %.3d\r\n", a, b, c);
	} else if (instruction_type[instr] == INSTR_iABx) {
	    GET_iABx(code->instructions+i, a, b);
	    if (instr == I_BRT || instr == I_JUMP)
		printf(" %.3d 0x%.3llX\r\n", a,
			(uint64_t) (code->instructions + i + b));
	    else
		printf(" %.3d %.3d\r\n", a, b);
	}
    }
    return 1;
}

uint64_t alm_dump_heap_item(ATERM t, int stack) {
  
  if (is_num(t))
      printf("%18.1lf ", num_val(t));
    else if (is_nil(t))
      printf("               [] ");
    else if (is_cons(t)) {
      printf("<cons/0x%.3llX> ", (uint64_t)cons_ptr(t));
    } else if (is_boxed(t)) {
      ATERM *box = boxed_ptr(t);
      if (is_atom(*box))
	printf("<atom/0x%.3llX> ", (uint64_t)box);
    } else if (is_header(t)) {
      if (stack) 
	printf("<fram/0x%.3llX> ",(uint64_t)frame_val(t));
      else if (is_atom(t)) {
	printf("<atom/0x%.3llX> ",boxed_arity(t));
	return boxed_arity(t)+1;
      } else 
	printf("<frwd/0x%.3llX> ",(uint64_t)frame_val(t));
    }

  return 1;
}

int alm_dump_heap(heap_t *h, int ylive, ATERM* rootset, int rootset_size) {
  int i = 0;
  ATERM *t = h->top;

  printf("Rootset size: %d ",rootset_size);
  printf("Stack size: %ld  ",h->stack-t+ylive);
  printf("Heap size: %ld\r\n",h->top+h->size - h->heap);
  
  for (i = 0; i < rootset_size; i++ ) {
    printf(" [%d]        : ",i);
    alm_dump_heap_item(rootset[i],0);
    printf("\r\n");
  }

  if (h->stack-t+ylive != 0) {
    printf("Stack dump:\r\n");
    
    while(t != h->stack+ylive) {
      printf(" 0x%.3llX: ",(uint64_t)t);
      t += alm_dump_heap_item(*t,1);
      printf("\r\n");
    }
  }
  
  if (h->top + h->size - h->heap != 0) {
    printf("Heap dump:\r\n");
    t = h->heap;
    
    while (t < (h->top + h->size)) {
      printf(" 0x%.3llX: ",(uint64_t)t);
      t += alm_dump_heap_item(*t,0);
      printf("\r\n");
    }
  }

  return 1;
}
