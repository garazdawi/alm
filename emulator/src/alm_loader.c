#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>

#include "alm_loader.h"

#define CHK(expr) do { if (expr) { printf("Failed on line %d\r\n",__LINE__); exit(1); } } while(0)

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

int load_constants(char **filebuf, code_t *new_code) {
    int i;

    new_code->num_constants = get_int32(*filebuf);
    new_code->constants = malloc(sizeof(uint32_t)*new_code->num_constants);
    (*filebuf) += 4;
    printf("Constants: %d\r\n",new_code->num_constants);

    for (i = 0; i < new_code->num_constants; i++) {
	int size = *((*filebuf)++);
	if (size == 4)
	    new_code->constants[i] = get_int32(*filebuf);
	else
	    new_code->constants[i] = 0;
	printf("  const[%d] : %d\r\n",i,new_code->constants[i]);
	(*filebuf) += size;
    }

    return 0;
}

int load_instructions(char **filebuf, code_t *new_code) {

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
    return 0;
}
