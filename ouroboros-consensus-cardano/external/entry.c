#include <stdlib.h>
#include "HsFFI.h"

HsBool mylib_init(void){
  int argc = 1;
  char *argv[] = { "snapshot-conversion", NULL };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);

  // do any other initialization here and
  // return false if there was a problem
  return HS_BOOL_TRUE;
}

void mylib_end(void){
  hs_exit();
}
