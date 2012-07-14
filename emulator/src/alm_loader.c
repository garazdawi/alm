#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>

#include "alm_instructions.h"
#include "alm_debug.h"
#include "alm_loader.h"

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

int load_constants(char **filebuf, code_t *new_code) {
    int i;

    new_code->num_constants = get_int32(*filebuf);
    new_code->constants = malloc(sizeof(uint32_t)*new_code->num_constants);
    (*filebuf) += 4;


    for (i = 0; i < new_code->num_constants; i++) {
	int size = *((*filebuf)++);
	if (size == 4)
	    new_code->constants[i] = get_int32(*filebuf);
	else
	    new_code->constants[i] = 0;

	(*filebuf) += size;
    }

    return 0;
}

int load_instructions(char **filebuf, code_t *new_code) {
    int i;

    new_code->num_instructions = get_int32(*filebuf);
    new_code->instructions = malloc(sizeof(uint32_t)*new_code->num_instructions);
    (*filebuf) += 4;

    for (i = 0; i < new_code->num_instructions; i++) {
	new_code->instructions[i] = get_int32(*filebuf);
	if (GET_INSTR(new_code->instructions+i) < 0 ||
		GET_INSTR(new_code->instructions+i) >= INSTR_COUNT) {
	    // Do a small sanity check
	    printf("Encountered invalid instruction 0x%X\r\n",new_code->instructions[i]);
	    exit(1);
	}
	(*filebuf) += 4;
    }

    return 0;
}

int load(code_t *new_code, char *filename) {
    struct stat st;
    int fd, i;
    char *filebuf, *fileptr;

    // File does not exist
    CHK(stat(filename, &st) != 0);

    // File open fails
    CHK((fd = open(filename, O_RDONLY)) == -1);

    filebuf = malloc(sizeof(char) * st.st_size);
    fileptr = filebuf;

    CHK(read(fd, filebuf, st.st_size) == -1);

    for (i = 0; i < st.st_size; i++) {
        if (i % 8 == 0)
            printf("\r\n");
        printf("%.2hhx ", filebuf[i]);
    }
    printf("\r\n");

    if (strncmp(filebuf, "alm", 3) != 0) {
        printf("File is not an alm file\r\n");
        return -1;
    }
    filebuf += 3;
    if (get_int32(filebuf) != 1) {
        printf("File is not the correct alm version\r\n");
        return -1;
    }
    filebuf += 4;
    CHK(load_constants(&filebuf,new_code) != 0);
    CHK(load_instructions(&filebuf,new_code) != 0);
    free(fileptr);

#ifdef DEBUG
    printf("Loaded new code from %s:\r\n",filename);
    alm_disasm(new_code);
#endif

    return 0;
}
