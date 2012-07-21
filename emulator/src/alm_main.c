#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "alm_debug.h"
#include "alm_term.h"
#include "alm_loader.h"
#include "alm_emu.h"

int parse_args(char** cmdline, ATERM *funcname, ATERM *args,int *arg_len) {
  char *str;
  int arg;
  *arg_len = 0;
  CHK((str = strsep(cmdline,"(")) == NULL);
  mk_atom(*funcname,str,strlen(str));

  while (*arg_len < 10 &&
	  (str = strsep(cmdline, ",)")) != NULL) {
    sscanf(str,"%d",&arg);
    *(args+*arg_len) = mk_num((double)arg);
    (*arg_len)++;
  }
  return 0;
}

int main (int argc, char** argv) {
    code_t code;
    ATERM args[10], funcname;
    int arg_len;

    if (load(&code, argv[1]) == -1)
        return 1;

    CHK(parse_args(argv+2,&funcname,args,&arg_len) != 0);

    CHK(process_main(&code,funcname,args,arg_len) != 0);

    return 0;
}
