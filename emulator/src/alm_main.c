#include <stdio.h>
#include <stddef.h>

#include "alm_loader.h"

int main (int argc, char** argv) {
    code_t code;
    if (load(&code, argv[1]) == -1)
        return 1;
    return 0;
}
