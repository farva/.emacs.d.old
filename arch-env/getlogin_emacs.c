#define _XOPEN_SOURCE
#define _GNU_SOURCE

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

static char LOGNAME_ARRAY[L_cuserid];
static char * (*real_getlogin)(void);

char * getlogin(void) {
  char * logname = getenv("LOGNAME");
  if (logname) {
    strcpy(LOGNAME_ARRAY, logname);
    /* LOGNAME_ARRAY[L_cuserid - 1] = '\0'; */
    return LOGNAME_ARRAY;
  } else {
    real_getlogin = dlsym(RTLD_NEXT, "getlogin");
    return real_getlogin();
  }
}
