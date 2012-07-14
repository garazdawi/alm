
#ifndef ALM_LOADER_H_
#define ALM_LOADER_H_

#include <stdint.h>

#include "alm_term.h"

typedef struct code {
    int num_constants, num_instructions;
    ATERM *constants;
    uint32_t *instructions;
} code_t;

int load(code_t *new_code, char *filename);


#endif /* ALM_LOADER_H_ */
