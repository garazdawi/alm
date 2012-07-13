typedef struct code {
    int num_constants, num_instructions;
    char *constants;
    char *instructions;
} code_t;

int load(code_t *new_code, char *filename);
