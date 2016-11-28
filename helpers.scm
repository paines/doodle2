
(import chicken scheme)
  (use (srfi 1 4 18) bind)
(bind-rename "getTimestamp" "get-timestamp")
(bind* #<<EOF
char* getTimestamp();
#ifndef CHICKEN
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <strings.h>
#endif

char* getTimestamp() {
                      char *buffer = (char*)malloc(32);
                      time_t t = time(0);
                      struct tm *tm;

                      tm = gmtime(&t);
                      strftime(buffer, 31, "%Y%m%d-%H%M%S", tm);
                      return buffer;
                     }

EOF
)
