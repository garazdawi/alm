#include <stdio.h>

#include "alm_loader.h"

int main (int argc, char** argv) {
    if (load(NULL, argv[1]) == -1)
        return 1;
    return 0;
}