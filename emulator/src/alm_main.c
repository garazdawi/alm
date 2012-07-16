#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "alm_debug.h"
#include "alm_term.h"
#include "alm_loader.h"
#include "alm_emu.h"

int parse_args(char** cmdline, ATERM *args,int *arg_len) {
  char *str;
  int arg;
  *arg_len = 0;
  CHK(strsep(cmdline,"(") == NULL);
  
  while (*arg_len < 10 &&
	  (str = strsep(cmdline, ",)")) != NULL) {
    sscanf(str,"%d",&arg);
    *(args+*arg_len) = (ATERM)arg;
    (*arg_len)++;
  }
  return 0;
}

int main (int argc, char** argv) {
    code_t code;
    ATERM args[10];
    int arg_len;

    if (load(&code, argv[1]) == -1)
        return 1;

    CHK(parse_args(argv+2,args,&arg_len) != 0);

    CHK(process_main(&code,args,arg_len) != 0);

    return 0;
}
