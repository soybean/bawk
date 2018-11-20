#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* This function converts a string to an int */
int string_to_int(char *a)
{
  a++;
  a[strlen(a)-1] = 0;
  return atoi(a);
}


char* int_to_string(int a) {
  int length = snprintf( NULL, 0, "%d", a);
  char* str = malloc( length + 1 );
  snprintf( str, length + 1, "%d", a );
  return str;
}
