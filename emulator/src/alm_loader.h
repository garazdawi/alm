
#ifndef ALM_LOADER_H_
#define ALM_LOADER_H_

#include <stdint.h>

#include "alm_term.h"

#define INSTR uint32_t

typedef struct function function_t;
struct function {
    function_t *next;
    int constant;
    INSTR *instruction;
};

typedef struct code {
    int num_constants, num_instructions;
    ATERM *constants;
    INSTR *instructions;
} code_t;

int load(code_t *new_code, char *filename);


#endif /* ALM_LOADER_H_ */
