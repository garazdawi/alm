
#ifndef ALM_LOADER_H_
#define ALM_LOADER_H_

#include <stdint.h>

typedef struct code {
    int num_constants, num_instructions;
    uint32_t *constants;
    uint32_t *instructions;
} code_t;

int load(code_t *new_code, char *filename);


#endif /* ALM_LOADER_H_ */
