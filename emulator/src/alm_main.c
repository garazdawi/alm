#include <stdio.h>
#include <stddef.h>

#include "alm_debug.h"
#include "alm_loader.h"
#include "alm_emu.h"

int main (int argc, char** argv) {
    code_t code;
    if (load(&code, argv[1]) == -1)
        return 1;
    CHK(process_main(&code) != 0);
    return 0;
}
