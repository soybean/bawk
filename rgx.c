/* Regex operators */

#include <regex.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

/* Returns 1 if string a matches rgx b, 0 otherwise */
int comp(char *a, char *b) {
  if (b[0] == '\'') {
    b++;
  }
  if (b[strlen(b) - 1] == '\'') {
    b[strlen(b) - 1] = 0;
  }
  regex_t regex;
  int comp = regcomp(&regex, b, REG_EXTENDED|REG_NOSUB);
  if (comp) {
    printf("Regex expression could not be compiled");
    return 0;
  }
  comp = regexec(&regex, a, 0, NULL, 0);
  if (!comp) {
    return 1;
  }
  return 0;
}

/* Returns 1 if string a doesn't match rgx b, 0 otherwise */
int ncomp(char *a, char *b) {
  int opp = comp(a, b);
  return !opp;
}

/* rgx a == rgx b */
int equals(char *a, char *b) {
  if (strcmp(a, b) == 0) {
    return 1;
  }
  return 0;
}

/* rgx a != rgx b */
int nequals(char *a, char *b) {
  return !equals(a, b);
}
