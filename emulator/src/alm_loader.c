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
    new_code->constants = malloc(sizeof(ATERM) * new_code->num_constants);
    (*filebuf) += 4;

    for (i = 0; i < new_code->num_constants; i++) {
	char type = *((*filebuf)++);
	uint64_t size = *((*filebuf)++);
	switch (type) {
	case 0:
	    new_code->constants[i] = mk_num((double)get_int32(*filebuf));
	    break;
	case 1:
	    mk_atom(new_code->constants[i], *filebuf, size);
	    break;
	case 2:
	    new_code->constants[i] = mk_nil();
	    break;
	}
	(*filebuf) += size;
    }

    return 0;
}

int load_instructions(char **filebuf, code_t *new_code) {
    int i, j, A, B, C;

    new_code->func_list = NULL;
    new_code->num_instructions = get_int32(*filebuf);
    new_code->instructions = malloc(sizeof(INSTR) * new_code->num_instructions);
    (*filebuf) += 4;

    for (i = 0; i < new_code->num_instructions; i++) {
	new_code->instructions[i] = get_int32(*filebuf);
	// Do a small sanity check
	if (GET_INSTR(new_code->instructions+i) < 0 ||
	GET_INSTR(new_code->instructions+i) >= INSTR_COUNT) {

	    printf("Encountered invalid instruction 0x%X\r\n",
		    new_code->instructions[i]);
	    exit(1);
	}
	switch(GET_INSTR(new_code->instructions+i)) {
	    case I_FUNC: {
		GET_iABC(new_code->instructions+i,A,B,C);
		function_t *func = malloc(sizeof(function_t));
		func->constant = A;
		func->instruction = new_code->instructions+i;
		func->next = new_code->func_list;
		new_code->func_list = func;
		break;
	    }
	    default:
	    break;
	}
	(*filebuf) += 4;
    }

    for (i = 0; i < new_code->num_instructions; i++) {

	switch(GET_INSTR(new_code->instructions+i)) {
	    case I_BRT :
	    case I_JUMP: {
		GET_iABx(new_code->instructions+i,A,B);
		for (j = 0; j < new_code->num_instructions; j++)
		if (GET_INSTR(new_code->instructions+j) == I_LABEL &&
			GET_Bx(new_code->instructions[j]) == B)
		SET_Bx(new_code->instructions[i],j-i);
		break;
	    }
	    default:
	    break;
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
    printf("Loaded new code from %s:\r\n", filename);
    alm_disasm(new_code);
#endif

    return 0;
}
