#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>

#include "alm_loader.h"

#define CHK(expr) if (expr) return -1

int load(char **buf, char *filename) {
    struct stat st;
    int fd;
    int i;

    // File does not exist
    CHK(stat(filename, &st) != 0);

    // File open fails
    CHK((fd = open(filename, O_RDONLY)) == -1);

    char *filebuf = malloc(sizeof(char) * st.st_size);

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
    if (strncmp(filebuf + 3, "\0\0\0\1") != 0) {
        printf("File is not the correct alm version\r\n");
        return -1;
    }
    return 0;
}